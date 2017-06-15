from .typevar import TypeVar
from .ast import Var

try:
    from typing import Dict, List, Tuple, TypeVar as MTypeVar
    from typing import Iterator, TYPE_CHECKING, cast, Set # noqa
    if TYPE_CHECKING:
        from srcgen import Formatter # noqa
        from .xform import Rtl # noqa
        from .instructions import Instruction # noqa
        from .ast import Expr, Def # noqa
        A = MTypeVar("A")
        B = MTypeVar("B")
        ErrLoc = Tuple[Rtl, int, bool, int]
        TypeMap = Dict[TypeVar, TypeVar]
        TypeEnv = Dict[Var, TypeVar]
except ImportError:
    pass


def azip(l1, l2):
    # type: (List[A], List[B]) -> Iterator[Tuple[A,B]]
    assert len(l1) == len(l2)
    return zip(l1, l2)


def resolve(tv, m):
    # type: (TypeVar, TypeMap) -> TypeVar
    while tv in m:
        tv1 = m[tv]
        assert tv != tv1
        tv = tv1

    if tv.is_derived:
        tv = TypeVar.derived(resolve(tv.base, m), tv.derived_func)

    return tv


def lookup(inst, defs, args):
    # type: (Instruction, List[A], List[A]) -> Tuple[TypeVar, A] # noqa
    (is_arg, idx) = inst.get_ctrl_typevar_ind()
    if is_arg:
        return (inst.ins[idx].typevar, args[idx])
    else:
        return (inst.outs[idx].typevar, defs[idx])


def subst(tv, m):
    # type: (TypeVar, TypeMap) -> TypeVar
    if tv in m:
        return m[tv]
    elif tv.is_derived:
        return TypeVar.derived(subst(tv.base, m), tv.derived_func)
    else:
        return tv


class TCError(Exception):
    def __init__(self, loc):
        # type: (ErrLoc) -> None
        super(TCError, self).__init__()
        self.loc = loc

    def set_loc(self, rtl, ln, is_arg, idx):
        # type: (TCError, Rtl, int, bool, int) -> None
        self.loc = (rtl, ln, is_arg, idx)

    def __repr__(self):
        # type: (TCError) -> str
        (rtl, line_no, is_arg, idx) = self.loc
        return "On line {} in {} no {} in '{}': ".format(
                line_no, "arg" if is_arg else "result",
                idx, rtl.rtl[line_no])

    def __eq__(self, other):
        # type: (TCError, object) -> bool
        return isinstance(other, TCError) and \
               self.__class__ == other.__class__ and \
               self.loc == other.loc

    def value(self):
        # type: (TCError) -> Expr
        (rtl, line_no, is_arg, idx) = self.loc
        if is_arg:
            return rtl.rtl[line_no].expr.args[idx]
        else:
            return rtl.rtl[line_no].defs[idx]


class TCUndef(TCError):
    def __repr__(self):
        # type: (TCUndef) -> str
        base = super(TCUndef, self).__repr__()
        return base + "Undefined type for {}".format(self.value())


class TCRedef(TCError):
    def __repr__(self):
        # type: (TCRedef) -> str
        base = super(TCRedef, self).__repr__()
        return base + "Redefining {}".format(self.value())


class TCNotSubtype(TCError):
    def __init__(self, loc, concr_type, formal_type):
        # type: (ErrLoc, TypeVar, TypeVar) -> None
        super(TCNotSubtype, self).__init__(loc)
        self.concr_type = concr_type
        self.formal_type = formal_type

    def __repr__(self):
        # type: (TCNotSubtype) -> str
        base = super(TCNotSubtype, self).__repr__()
        return base + "{} is not a subtype of {} for argument {}".format(
                self.concr_type, self.formal_type, self.value())
    # TODO: Override __eq__ to check inferred concrete and formal types
    # equalities


def _mk_loc(line_loc, arg_loc):
    # type: (Tuple[Rtl, int], Tuple[bool, int]) -> ErrLoc
    return (line_loc[0], line_loc[1], arg_loc[0], arg_loc[1])


def tc_def(d, m, line_loc):
    # type: (Def, TypeMap, Tuple[Rtl, int]) -> TypeMap
    inst = d.expr.inst
    defs = d.defs
    args = cast(List[Var], d.expr.args)

    formal_args = [x.typevar for x in inst.ins]
    actual_args = [x.get_typevar() for x in args]

    formal_defs = [x.typevar for x in inst.outs]
    actual_defs = [x.get_typevar() for x in defs]
    m1 = {}

    # Iff inst.ctrl_typevar exists, inst is polymorphic
    if hasattr(inst, "ctrl_typevar"):
        # Determing which argument is the controling TV
        arg_loc = inst.get_ctrl_typevar_ind()
        # Get the formal and actual control TVs
        formal_ctrl_tv, actual_ctrl_tv = \
            lookup(inst, actual_defs, actual_args)

        # If we don't have a type inferred/specified for
        # the actual controlling TV then the input is
        # underspecified
        if (actual_ctrl_tv not in m):
            raise TCUndef(_mk_loc(line_loc, arg_loc))

        actual_ctrl_tv = resolve(actual_ctrl_tv, m)
        assert not formal_ctrl_tv.is_derived

        actual_ctrl_ts = actual_ctrl_tv.get_typeset()
        formal_ctrl_ts = formal_ctrl_tv.get_typeset()

        # The actual control variable's typeset violates
        # the instruction signature
        if not (actual_ctrl_ts.is_subset(formal_ctrl_ts)):
            raise TCNotSubtype(_mk_loc(line_loc, arg_loc),
                               actual_ctrl_tv,
                               formal_ctrl_tv)

        m1[formal_ctrl_tv] = actual_ctrl_tv
    else:
        assert not inst.is_polymorphic
        actual_ctrl_tv = None

    # print "Args: "
    for (ind, (form, actual)) in enumerate(azip(formal_args, actual_args)):
        arg_loc = (True, ind)
        if (actual_ctrl_tv == actual):
            continue

        actual = resolve(actual, m)
        form = resolve(subst(form, m1), m)

        if (not actual.get_typeset().is_subset(form.get_typeset())):
            raise TCNotSubtype(_mk_loc(line_loc, arg_loc), actual, form)

    for (ind, (form, actual)) in enumerate(azip(formal_defs, actual_defs)):
        arg_loc = (False, ind)
        if actual_ctrl_tv == actual:
            continue

        form = resolve(subst(form, m1), m)
        if actual in m:
            raise TCRedef(_mk_loc(line_loc, arg_loc))
        m[actual] = form

    return m


def tc_rtl(r, m):
    # type: (Rtl, TypeMap) -> TypeMap
    for ln, d in enumerate(r.rtl):
        m = tc_def(d, m, (r, ln))

    return m


def to_TypeMap(t):
    # type: (TypeEnv) -> TypeMap
    return {k.get_typevar(): v for (k, v) in t.items()}


def io_shape(r):
    # type: (Rtl) -> Tuple[Set[Expr], Set[Expr], Set[Expr]]
    inputs = set()  # type: Set[Expr]
    input_ctrls = set()  # type: Set[Expr]
    defs = set()  # type: Set[Expr]
    for d in r.rtl:
        inst = d.expr.inst
        actual_uses = list(d.expr.args)  # type: List[Expr]
        actual_defs = list(d.defs)  # type: List[Expr]

        inputs = inputs.union(set(actual_uses).difference(defs))
        if hasattr(inst, "ctrl_typevar"):
            # Get the formal and actual control TVs
            _, actual_ctrl = lookup(inst, actual_defs, actual_uses)
            if actual_ctrl not in defs:
                input_ctrls.add(actual_ctrl)

        defs = defs.union(actual_defs)

    return (inputs, input_ctrls, defs)
