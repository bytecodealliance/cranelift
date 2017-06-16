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


class TCError(Exception):
    """
    Based TypeCheck exception class
    """
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
    Error raised when the initial type environment does not include a type for
    all free variables that determine the monomorphic type of some instruction.
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
    Error raised when the initial type environment includes extra unneccessary
    definitions.
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
    Error raised when the RTL redefines an already-defined variable. Could be
    because its not in SSA form?
    """
    def __repr__(self):
        # type: (TCRedef) -> str
        base = super(TCRedef, self).__repr__()
        return base + "Redefining {}".format(self.expr())


class TCNotSubtype(TCError):
    """
    Error raised when the actual type of a variable doesn't agree with the
    formal type of an instruction where it is used/produced.
    """
    def __init__(self, loc, concr_type, formal_type):
        # type: (ErrArgLoc, TypeVar, TypeVar) -> None
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
    """
    Error raised when the inferred types for a variable for the src and dst
    patterns differ. Applies only when type-checking XForms.
    """
    def __init__(self, loc, outp, src_t, dst_t):
        # type: (XForm, Var, TypeVar, TypeVar) -> None
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


# Helper Functions
def _mk_loc(line_loc, arg_loc):  # noqa
    # type: (Tuple[Rtl, int], Tuple[bool, int]) -> ErrArgLoc
    return (line_loc[0], line_loc[1], arg_loc[0], arg_loc[1])


def agree(actual_typ, formal_typ):
    # type: (TypeVar, TypeVar) -> bool
    return actual_typ.get_typeset().is_subset(formal_typ.get_typeset())


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


def io_shape(r):
    # type: (Rtl) -> Tuple[Set[Var], Set[Var], Set[Var], Set[Var]]
    """ Given an Rtl r compute the tuple (inputs, ctrls, free_nd_tvs, defs) where:

            - inputs is the set of Vars that are free in r
            - ctrls is the set of control variables free in r
            - free_nd_tvs is the set of free type vars in r that don't depend
                on a control variable
            - defs is the set of variables defined in r
    """
    inputs = set()  # type: Set[Var]
    input_ctrls = set()  # type: Set[Var]
    free_nonderiv_tvs = set()
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
                    free_nonderiv_tvs.add(actual_args[idx])

        defs = defs.union(actual_defs)

    return (inputs, input_ctrls, free_nonderiv_tvs, defs)


# Typechecking Functions
def tc_def(d, env, line_loc):  # noqa
    # type: (Def, TypeEnv, Tuple[Rtl, int]) -> TypeEnv
    """
    Type-check a Def d in a type environment env. Return a new type environment
    env' enriched with any defs/uses not specified in the inital type env.

    If d invokes a polymorphic instruction env MUST include a type for it.
    If d involves uses of free type variables not derived from a control tv,
    env MAY include types for those. Otherwise those types will be inferred to
    be corresponding type in the instruction's polymorphic signature.
    """
    inst = d.expr.inst
    formal_defs = list(inst.outs)  # type: List[Operand]
    actual_defs = list(d.defs)  # type: List[Var]

    formal_args = list(inst.ins)  # type: List[Operand]
    actual_args = cast(List[Var], list(d.expr.args))

    monomorph_map = {}  # type: TypeMap

    if inst.is_polymorphic:
        # Get the formal and actual control TVs
        formal_ctrl_tv, actual_ctrl_exp = \
            lookup(inst, actual_defs, actual_args)

        actual_ctrl_tv = env[actual_ctrl_exp]
        assert not formal_ctrl_tv.is_derived

        # Check if hte actual control variable is a subset of the formal
        # control variable
        if not (agree(actual_ctrl_tv, formal_ctrl_tv)):
            raise TCNotSubtype(_mk_loc(line_loc, inst.get_ctrl_typevar_ind()),
                               actual_ctrl_tv,
                               formal_ctrl_tv)

        monomorph_map[formal_ctrl_tv] = actual_ctrl_tv
    else:
        actual_ctrl_exp = None

    # Check that each actual's type agrees with the corresponding
    # (monomorphised) formal type
    for (ind, (form, actual)) in enumerate(azip(formal_args, actual_args)):
        if (actual_ctrl_exp == actual):
            continue  # skip the control variable - already checked

        if (actual in env):
            actual_typ = env[actual]
        else:
            actual_typ = subst(form.typevar, monomorph_map)
            env[actual] = actual_typ

        formal_typ = subst(form.typevar, monomorph_map)

        if (not agree(actual_typ, formal_typ)):
            raise TCNotSubtype(_mk_loc(line_loc, (True, ind)),
                               actual_typ,
                               formal_typ)

    # Set the type of each def based on the (monomorphised) instruction
    # signature
    for (ind, (form, actual)) in enumerate(azip(formal_defs, actual_defs)):
        if actual_ctrl_exp == actual:
            continue  # skip the control variable - already checked

        form_typ = subst(form.typevar, monomorph_map)

        # We shouldn't be re-defining values since we're in SSA form
        # and insist consumers only provide types for the control vars
        if actual in env:
            raise TCRedef(_mk_loc(line_loc, (False, ind)))

        env[actual] = form_typ

    return env


def tc_rtl(r, m):
    # type: (Rtl, TypeEnv) -> TypeEnv
    """
    Typecheck an Rtl r in a type environment m. Returns an updated type
    environment m' that includes types for all definitions and uses in r.

    The input environment m should include a type for all free control
    variables.  It can also include types for any free type variables that are
    not derived from a control variable.
    """
    (inputs, controls, free_nd_tvs, defs) = io_shape(r)

    # Let S be the vars defined in the starting type env m
    # Let C be the vars in the controls set, and F be the vars in the
    # free_nd_tvs set.
    # Check that C < S < C + F where < is subset and + is union.
    user_provided = set(m.keys())

    if not (user_provided.issuperset(controls)):
        # User didn't specify a control variable
        undef = controls.difference(user_provided)
        raise TCUnderspecified(r, undef)

    if not (user_provided.issubset(controls.union(free_nd_tvs))):
        # User provided unneccessary initial type
        extra = user_provided.difference(controls.union(free_nd_tvs))
        raise TCOverspecified(r, extra)

    for ln, d in enumerate(r.rtl):
        m = tc_def(d, m, (r, ln))

    # At the end, the type environment should contain
    # a type for every input, controlvar and def
    all_vars = inputs.union(controls).union(defs).union(free_nd_tvs)
    inferred_vars = set(m.keys())
    assert all_vars == inferred_vars

    return m


def tc_xform(x, m):
    # type: (XForm, TypeEnv) -> TypeEnv
    """
    Typecheck a XForm x in a type environment m. Returns an updated
    type-environment m' that includes types for all definitions/uses
    in x.src and x.dst.
    """
    user_provided = set(m.keys())

    # Determing what variables in the initial type env m apply to each rtl.
    _, src_ctrl, src_free_nd_tvs, _ = io_shape(x.src)
    _, dst_ctrl, dst_free_nd_tvs, _ = io_shape(x.dst)
    src_vars = src_ctrl.union(src_free_nd_tvs)
    dst_vars = dst_ctrl.union(dst_free_nd_tvs)

    # Filter out the parts of m that apply for src and dst in src_m and dst_m
    # respectively
    src_m = {k: v for (k, v) in m.items() if k in src_vars}
    dst_m = {k: v for (k, v) in m.items() if k in dst_vars}

    # Typecheck each rtl
    src_m = tc_rtl(x.src, src_m)
    dst_m = tc_rtl(x.dst, dst_m)

    # If we encounter any:
    #   - free non-derived TVs
    #   - for which the user didn't specify a type
    #   - that appear in both src and dst
    #   - whose inferred types in src and dst differ
    #
    # Then infer their type to be the intersection of the types in src and dst
    for v in dst_free_nd_tvs.difference(user_provided):
        assert v in src_free_nd_tvs and v in x.inputs
        src_tv = src_m[v]
        dst_tv = dst_m[v]

        if (not (src_tv == dst_tv)):
            intersection_tv = src_tv.intersection(dst_tv)
            src_m[v] = intersection_tv
            dst_m[v] = intersection_tv

    # Check that the final type envs agree on the types of all
    # inputs
    for v in x.inputs:
        if (v not in src_m or v not in dst_m):
            continue

        if (not(src_m[v] == dst_m[v])):
            raise TCDisagree(x, v, src_m[v], dst_m[v])

    # Check that the final type envs agree on the types of all
    # defs that appear on both sides
    for v in x.defs:
        if not v.is_output():
            continue
        # Patterns must agree on the types of the
        # outputs.
        if (src_m[v] != dst_m[v]):
            raise TCDisagree(x, v, src_m[v], src_m[v])

    # Unify the two final type envs
    for v in dst_m:
        if v in src_m:
            assert dst_m[v] == src_m[v]
        else:
            src_m[v] = dst_m[v]

    return src_m
