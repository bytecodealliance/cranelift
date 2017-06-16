from .typevar import TypeVar
from .ast import Var
from .xform import Rtl, XForm

try:
    from typing import Dict, List, Tuple, TypeVar as MTypeVar
    from typing import Iterator, TYPE_CHECKING, cast, Set, Union  # noqa
    ErrArgLoc = Tuple[Rtl, int, bool, int]
    if TYPE_CHECKING:
        from .instructions import Instruction  # noqa
        from .ast import Expr, Def  # noqa
        from .operands import Operand  # noqa
        A = MTypeVar("A")
        B = MTypeVar("B")
        ErrLoc = Union[Rtl, XForm, ErrArgLoc]
        TypeMap = Dict[TypeVar, TypeVar]
        TypeEnv = Dict[Var, TypeVar]
except ImportError:
    pass


def azip(l1, l2):
    # type: (List[A], List[B]) -> Iterator[Tuple[A,B]]
    assert len(l1) == len(l2)
    return zip(l1, l2)


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
        if (isinstance(self.loc, tuple)):
            (rtl, line_no, is_arg, idx) = self.loc
            return "On line {} in {} no {} in '{}': ".format(
                    line_no, "arg" if is_arg else "result",
                    idx, rtl.rtl[line_no])
        elif (isinstance(self.loc, Rtl)):
            return "In RTL {}: ".format(self.loc)
        else:
            assert isinstance(self.loc, XForm)
            return "In XForm {}: ".format(self.loc)

    def __eq__(self, other):
        # type: (TCError, object) -> bool
        return isinstance(other, TCError) and \
               self.__class__ == other.__class__ and \
               self.loc == other.loc

    def __hash__(self):
        # type: (TCError) -> int
        return Exception.__hash__(self)

    def expr(self):
        # type: (TCError) -> Expr
        assert isinstance(self.loc, tuple) and len(self.loc) == 4
        (rtl, line_no, is_arg, idx) = cast(ErrArgLoc, self.loc)
        if is_arg:
            return rtl.rtl[line_no].expr.args[idx]
        else:
            return rtl.rtl[line_no].defs[idx]

#  Type errors due to bad initial type environment
class TCUnderspecified(TCError): # noqa
    """
    The initial type environment does not include a type for all free variables
    that determine the monomorphic type of some instruction.
    """
    def __init__(self, rtl, missing):
        # type: (Rtl, Set[Var]) -> None
        super(TCUnderspecified, self).__init__(rtl)
        self.missing = missing

    def __repr__(self):
        # type: (TCUnderspecified) -> str
        base = super(TCUnderspecified, self).__repr__()
        return base + "Missing initial types {}".format(self.missing)

    def __eq__(self, other):
        # type: (TCUnderspecified, object) -> bool
        return super(TCUnderspecified, self).__eq__(other) and \
                self.missing == cast(TCUnderspecified, other).missing

    def __hash__(self):
        # type: (TCUnderspecified) -> int
        return Exception.__hash__(self)


class TCOverspecified(TCError):
    """
    The initial type environment includes extra unneccessary definitions.
    """
    def __init__(self, rtl, extra):
        # type: (Rtl, Set[Var]) -> None
        super(TCOverspecified, self).__init__(rtl)
        self.extra = extra

    def __repr__(self):
        # type: (TCOverspecified) -> str
        base = super(TCOverspecified, self).__repr__()
        return base + "Unneccessary initial types {}".format(self.extra)

    def __eq__(self, other):
        # type: (TCOverspecified, object) -> bool
        return super(TCOverspecified, self).__eq__(other) and \
                self.extra == cast(TCOverspecified, other).extra

    def __hash__(self):
        # type: (TCOverspecified) -> int
        return Exception.__hash__(self)


#  Type errors due to bad Rtl
class TCRedef(TCError):
    """
    The RTL redefines an already-defined variable. Could be because its not in
    SSA form?
    """
    def __repr__(self):
        # type: (TCRedef) -> str
        base = super(TCRedef, self).__repr__()
        return base + "Redefining {}".format(self.expr())


class TCNotSubtype(TCError):
    """
    The actual type of a variable doesn't agree with the formal type of the
    instruction to which its passed in. There are 2 distinct causes:
        1) A contorl type var doesn't agree with formal type of the instruction
        2) Another input variable doesn't agree with the monomorphised type of
           the instruction its being passed to
    """
    def __init__(self, loc, concr_type, formal_type):
        # type: (ErrLoc, TypeVar, TypeVar) -> None
        super(TCNotSubtype, self).__init__(loc)
        self.concr_type = concr_type
        self.formal_type = formal_type

    def __repr__(self):
        # type: (TCNotSubtype) -> str
        base = super(TCNotSubtype, self).__repr__()
        return base + "{} is not a subtype of {} for {}".format(
                self.concr_type, self.formal_type, self.expr())
    # TODO: Override __eq__ to check inferred concrete and formal types
    # equalities

# Type errors across 2 patterns in a transform
class TCDisagree(TCError): # noqa
    def __init__(self, loc, outp, src_t, dst_t):
        # type: (ErrLoc, Var, TypeVar, TypeVar) -> None
        super(TCDisagree, self).__init__(loc)
        self.bad_outp = outp
        self.src_t = src_t
        self.dst_t = dst_t

    def __repr__(self):
        # type: (TCDisagree) -> str
        base = super(TCDisagree, self).__repr__()
        return base +\
            "Types of {} disagree between patterns - {} in src and {} in dest"\
            .format(self.bad_outp, self.src_t, self.dst_t)


class TCUnderconstrainedInput(TCError):
    def __init__(self, loc, inp):
        # type: (ErrLoc, Var) -> None
        super(TCUnderconstrainedInput, self).__init__(loc)
        self.inp = inp

    def __repr__(self):
        # type: (TCUnderconstrainedInput) -> str
        base = super(TCUnderconstrainedInput, self).__repr__()
        return base + "Input {} not used in one of the patterns."\
            .format(self.inp)


def _mk_loc(line_loc, arg_loc):
    # type: (Tuple[Rtl, int], Tuple[bool, int]) -> ErrLoc
    return (line_loc[0], line_loc[1], arg_loc[0], arg_loc[1])


def agree(actual_typ, formal_typ):
    # type: (TypeVar, TypeVar) -> bool
    print ("agree?({}[{}], {}[{}])".format(
           actual_typ, actual_typ.get_typeset(),
           formal_typ, formal_typ.get_typeset()))
    return actual_typ.get_typeset().is_subset(formal_typ.get_typeset())


def tc_def(d, env, line_loc):
    # type: (Def, TypeEnv, Tuple[Rtl, int]) -> TypeEnv
    inst = d.expr.inst
    formal_defs = list(inst.outs)  # type: List[Operand]
    actual_defs = list(d.defs)  # type: List[Var]

    formal_args = list(inst.ins)  # type: List[Operand]
    actual_args = cast(List[Var], list(d.expr.args))

    m1 = {}  # type: TypeMap

    # Iff inst.ctrl_typevar exists, inst is polymorphic
    if inst.is_polymorphic:
        # Get the formal and actual control TVs
        formal_ctrl_tv, actual_ctrl_exp = \
            lookup(inst, actual_defs, actual_args)

        actual_ctrl_tv = env[actual_ctrl_exp]
        assert not formal_ctrl_tv.is_derived

        # The actual control variable's typeset violates
        # the instruction signature
        if not (agree(actual_ctrl_tv, formal_ctrl_tv)):
            raise TCNotSubtype(_mk_loc(line_loc, inst.get_ctrl_typevar_ind()),
                               actual_ctrl_tv,
                               formal_ctrl_tv)

        m1[formal_ctrl_tv] = actual_ctrl_tv
    else:
        actual_ctrl_exp = None

    # Check that each actual's type agrees with the corresponding
    # (monomorphised) formal type
    for (ind, (form, actual)) in enumerate(azip(formal_args, actual_args)):
        if (actual_ctrl_exp == actual):
            continue

        if (actual in env):
            actual_typ = env[actual]
        else:
            # This is the case when this input variable is inferred from
            # the control var
            actual_typ = subst(form.typevar, m1)
            env[actual] = actual_typ
        formal_typ = subst(form.typevar, m1)

        if (not agree(actual_typ, formal_typ)):
            raise TCNotSubtype(_mk_loc(line_loc, (True, ind)),
                               actual_typ,
                               formal_typ)

    # Set the type of each def based on the (monomorphised) instruction
    # signature
    for (ind, (form, actual)) in enumerate(azip(formal_defs, actual_defs)):
        if actual_ctrl_exp == actual:
            continue

        form_typ = subst(form.typevar, m1)

        # We shouldn't be re-defining values since we're in SSA form
        # and insist consumers only provide types for the control vars
        if actual in env:
            raise TCRedef(_mk_loc(line_loc, (False, ind)))

        env[actual] = form_typ

    return env


def tc_rtl(r, m):
    # type: (Rtl, TypeEnv) -> TypeEnv
    (inputs, controls, defs) = io_shape(r)

    # Expect the starting type enviroment to contain
    # types precisely for the control relevant variables.
    user_provided = set(m.keys())

    if controls != user_provided:
        undef = controls.difference(user_provided)
        if (len(undef) > 0):
            # User didn't specify a control variable
            raise TCUnderspecified(r, undef)

        extra = user_provided.difference(controls)
        assert (len(extra) > 0)
        raise TCOverspecified(r, extra)

    for ln, d in enumerate(r.rtl):
        m = tc_def(d, m, (r, ln))

    # At the end, the type environment should contain
    # a type for every input, controlvar and def
    all_vars = inputs.union(controls).union(defs)
    inferred_vars = set(m.keys())
    assert all_vars == inferred_vars

    return m


def tc_xform(x, m):
    # type: (XForm, TypeEnv) -> TypeEnv
    #  Rewrite m using the internal Var versions of the transform x. Some
    #  control variables may appear only in one of the patterns (e.g.
    #  temporary/intermediate defs on the LHS of iconst, uextend etc.).  Make
    #  sure ot filter out for src/dst only the variables from m that appear
    #  as control vars in src/dst respectively
    _, src_ctrl, _ = io_shape(x.src)
    _, dst_ctrl, _ = io_shape(x.dst)

    src_m = {k: v for (k, v) in m.items() if k in src_ctrl}
    dst_m = {k: v for (k, v) in m.items() if k in dst_ctrl}

    src_m = tc_rtl(x.src, src_m)
    dst_m = tc_rtl(x.dst, dst_m)

    for v in x.inputs:
        if (v not in src_m or v not in dst_m):
            raise TCUnderconstrainedInput(x, v)

        if (src_m[v] != dst_m[v]):
            raise TCDisagree(x, v, src_m[v], dst_m[v])

    for v in x.defs:
        if not v.is_output():
            continue
        # Patterns must agree on the types of the
        # outputs.
        if (src_m[v] != dst_m[v]):
            raise TCDisagree(x, v, src_m[v], src_m[v])

    for v in dst_m:
        if v in src_m:
            assert dst_m[v] == src_m[v]
        else:
            src_m[v] = dst_m[v]

    return src_m


def io_shape(r):
    # type: (Rtl) -> Tuple[Set[Var], Set[Var], Set[Var]]
    """ Given an Rtl r compute the triple (inputs, ctrls, defs) where:
            inputs is the set of Vars that are free in r
            ctrls is the set of control variables free in r + the set of
                type vars that don't depend on control variables
            defs is the set of variables defined in r
    """
    inputs = set()  # type: Set[Var]
    input_ctrls = set()  # type: Set[Var]
    defs = set()  # type: Set[Var]
    for d in r.rtl:
        inst = d.expr.inst

        # Expect all arguments to be just Variables
        for u in d.expr.args:
            assert isinstance(u, Var)

        actual_args = cast(List[Var], list(d.expr.args))
        actual_defs = list(d.defs)  # type: List[Var]

        inputs = inputs.union(set(actual_args).difference(defs))
        if inst.is_polymorphic:
            # Get the formal and actual control TVs
            formal_ctrl, actual_ctrl = lookup(inst, actual_defs, actual_args)
            if actual_ctrl not in defs:
                input_ctrls.add(actual_ctrl)

            for (idx, op) in enumerate(inst.ins):
                if (not hasattr(op, 'typevar')):
                    continue

                op_tv = op.typevar
                if (op_tv.get_nonderived_base() != formal_ctrl):
                    # This operand doesn't depend on the control var (e.g.
                    # bint, uextend)
                    input_ctrls.add(actual_args[idx])

        defs = defs.union(actual_defs)

    return (inputs, input_ctrls, defs)
