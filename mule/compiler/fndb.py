"""The known-function database for Python operations, following ``fndb.lisp``.

Names beginning with ``%`` are compiler-internal operations that IR1
conversion emits for Python syntax and for Mule's runtime protocols. Their
attributes are deliberately conservative: nearly every Python operator can run
user code and raise, so almost nothing is flushable or foldable until the
optimizer proves operand types.
"""

from __future__ import annotations

from contextlib import contextmanager
from threading import local

from mule import ast

from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BYTES_TYPE,
    EMPTY_LIST_TYPE,
    FLOAT,
    FUNCTION_TYPE,
    INTEGER,
    NONE_TYPE,
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    PythonIntegerType,
    TextType,
    TupleType,
    ValueType,
)
from .knownfun import defknown, fun_info
from .node import CallKind, LeafVisitor, NodeVisitor


class _DerivationMode(local):
    def __init__(self):
        self.active = False


_DERIVATION_MODE = _DerivationMode()


@contextmanager
def flow_propagation():
    """Defer operand-dependent ``derive_type`` hooks while ``constraint.py``
    propagates per-context flow facts and user-code invalidation."""
    previous = _DERIVATION_MODE.active
    _DERIVATION_MODE.active = True
    try:
        yield
    finally:
        _DERIVATION_MODE.active = previous


class _LiteralType(ast.Visitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_NoneLiteral(self, node):
        return NONE_TYPE

    def visit_BooleanLiteral(self, node):
        return BOOLEAN_TRUE if node.value else BOOLEAN_FALSE

    def visit_IntegerLiteral(self, node):
        return IntegerType(node.value, node.value)

    def visit_FloatLiteral(self, node):
        return FLOAT

    def visit_TextLiteral(self, node):
        return (
            TextType(node.value)
            if node.value == "" or (len(node.value) == 1 and ord(node.value) <= 0xFF)
            else TEXT_TYPE
        )

    def visit_BytesLiteral(self, node):
        return BYTES_TYPE

    def visit_Tuple(self, node):
        arity = len(node.elements)
        if 0 < arity <= 64:
            elements = tuple(el.accept(self) for el in node.elements)
            if not all(el is UNKNOWN for el in elements):
                return TupleType(arity, elements)
        return TupleType(arity)


_LITERAL_TYPE = _LiteralType()


class _DeriveUseType(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return leaf.type if leaf.type is not None else UNKNOWN

    def visit_Constant(self, leaf):
        return leaf.value.accept(_LITERAL_TYPE)

    def visit_Lambda(self, leaf):
        return FUNCTION_TYPE

    def visit_LambdaVar(self, leaf):
        if leaf.type is not None:
            return leaf.type
        if leaf.optimistic_type is not None and leaf.optimistic_type is not BOTTOM:
            return leaf.optimistic_type
        return UNKNOWN

    def visit_Cast(self, node):
        if node.value is None:
            return UNKNOWN
        return node.contract.refine(_arg_type(node.value))

    def visit_Combination(self, node):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.derive_type is not None
        ):
            derived = node.fun_info.derive_type(node)
            if derived is not None:
                return derived
        return UNKNOWN

    def visit_Exit(self, node):
        return _arg_type(node.value) if node.value is not None else UNKNOWN


_DERIVE_USE_TYPE = _DeriveUseType()


def _use_type(node) -> ValueType:
    if node.derived_type is not None:
        return node.derived_type
    return node.accept(_DERIVE_USE_TYPE)


def _arg_type(lvar) -> ValueType:
    if lvar.derived_type is not None:
        return lvar.derived_type
    if not lvar.uses:
        return BOTTOM
    derived = _use_type(lvar.uses[0])
    for use in lvar.uses[1:]:
        derived = derived.join(_use_type(use))
    return derived


def _derive_getslice(node) -> ValueType | None:
    if _DERIVATION_MODE.active:
        return None
    if any(_arg_type(arg).may_be_unbound for arg in node.args):
        return UNKNOWN
    arg_types = [_arg_type(arg) for arg in node.args[1:4]]
    return _arg_type(node.args[0]).getslice_type(*arg_types)


def _derive_build_list(node) -> ValueType | None:
    if _DERIVATION_MODE.active:
        return None
    result: ValueType = EMPTY_LIST_TYPE
    for arg in node.args:
        result = result.with_element(_arg_type(arg))
    return result


def _derive_build_tuple(node) -> ValueType | None:
    if _DERIVATION_MODE.active:
        return None
    arity = len(node.args)
    if 0 < arity <= 64:
        elements = tuple(_arg_type(arg) for arg in node.args)
        if not all(el is UNKNOWN for el in elements):
            return TupleType(arity, elements)
    return TupleType(arity)


def _derive_unpack(node) -> ValueType | None:
    if _DERIVATION_MODE.active:
        return None
    arity = _arg_type(node.args[1]).exact_integer()
    if arity is not None and arity >= 0:
        seq_type = _arg_type(node.args[0])
        if seq_type.fits_tuple(TupleType(arity)):
            return seq_type
        return TupleType(arity)
    return TupleType(None)


# Binary operators: one known per operator and one per in-place form.
BINARY = {
    operator: defknown(
        f"%{operator.name.lower()}",
        "call",
        derive_type=lambda node, op=operator: (
            None
            if _DERIVATION_MODE.active
            else _arg_type(node.args[0]).binary(op, _arg_type(node.args[1]))
        ),
        numeric=True,
    )
    for operator in ast.BinaryOperator
}
INPLACE_BINARY = {
    operator: defknown(
        f"%i{operator.name.lower()}",
        "call",
        "unsafe",
        derive_type=lambda node, op=operator: (
            None
            if _DERIVATION_MODE.active
            else _arg_type(node.args[0]).binary(op, _arg_type(node.args[1]))
        ),
        numeric=True,
    )
    for operator in ast.BinaryOperator
}
UNARY = {
    operator: defknown(
        f"%unary_{operator.name.lower()}",
        "call",
        derive_type=lambda node, op=operator: (
            None if _DERIVATION_MODE.active else _arg_type(node.args[0]).unary(op)
        ),
        numeric=(operator is not ast.UnaryOperator.NOT),
    )
    for operator in ast.UnaryOperator
}
COMPARE = {
    operator: defknown(
        f"%compare_{operator.name.lower()}",
        "call",
        derive_type=lambda node, op=operator: (
            None
            if _DERIVATION_MODE.active
            else _arg_type(node.args[0]).comparison(op, _arg_type(node.args[1]))
        ),
        numeric=(
            operator
            not in (
                ast.ComparisonOperator.IS,
                ast.ComparisonOperator.IS_NOT,
                ast.ComparisonOperator.IN,
                ast.ComparisonOperator.NOT_IN,
            )
        ),
        borrowed_args=(
            frozenset({0, 1})
            if operator in (ast.ComparisonOperator.IS, ast.ComparisonOperator.IS_NOT)
            else frozenset()
        ),
    )
    for operator in ast.ComparisonOperator
}

# Attribute and method protocols.
GETATTR = defknown("%getattr", "call", non_integer_first_arg=True)
# ``getattr(x, "name", default)`` and ``hasattr(x, "name")`` with a constant
# name: the builtin call, rewritten once the name is known (seqtran).
GETATTR_DEFAULT = defknown("%getattr_default", "call", non_integer_first_arg=True)
HASATTR = defknown(
    "%hasattr", "call", derive_type=lambda node: BOOLEAN, non_integer_first_arg=True
)
# The builtins themselves, so a call to an unmutated global ``getattr`` or
# ``hasattr`` reaches the transforms above, as ``len`` does. They are never
# emitted as known calls: without a constant name the call stays generic.
GETATTR_BUILTIN = defknown("getattr", "call", "unsafe")
HASATTR_BUILTIN = defknown("hasattr", "call", "unsafe")


def _isinstance_answer(node) -> ValueType | None:
    """``True`` when the operand's class is proven to be one of the classes
    or a subclass. ``False`` is never derived: a metaclass ``__instancecheck__``
    (``abc.ABCMeta`` with registered virtual subclasses) can accept an
    instance whose class is unrelated, and the compiler cannot see that."""
    if _DERIVATION_MODE.active:
        return None
    value, *classes = (_arg_type(arg) for arg in node.args)
    if any(value.is_instance_of(cls) is True for cls in classes):
        return BOOLEAN_TRUE
    return BOOLEAN


def _constrain_instance_check(visitor, node, state, truth: bool) -> bool:
    """Narrow the tested variable on each branch. True narrows to the union of
    the matching classes; false removes each of them. A true branch the
    classes would make impossible is left unnarrowed, not dead: a metaclass
    ``__instancecheck__`` may accept a virtual subclass."""
    source = visitor.origins.get(node.args[0])
    if source is None or source not in visitor.state:
        return False
    classes = [visitor.lvar_fact(arg).type for arg in node.args[1:]]
    if not all(cls.fits_class() for cls in classes):
        # A nested tuple or non-class operand: no class to narrow by.
        return False
    current = visitor.state[source]
    if truth:
        narrowed = BOTTOM
        for cls in classes:
            narrowed = narrowed.join(current.type.narrow_by_runtime_class(cls, True))
        if narrowed is BOTTOM:
            return True
    else:
        narrowed = current.type
        for cls in classes:
            narrowed = narrowed.narrow_by_runtime_class(cls, False)
    if narrowed is not current.type:
        state[source] = visitor.narrowed_fact(current, narrowed)
        for alias in visitor.walk.aliases.get(source, ()):
            if alias not in visitor.walk.indirect and alias in state:
                alias_current = state[alias]
                if truth:
                    alias_narrowed = BOTTOM
                    for cls in classes:
                        alias_narrowed = alias_narrowed.join(
                            alias_current.type.narrow_by_runtime_class(cls, True)
                        )
                else:
                    alias_narrowed = alias_current.type
                    for cls in classes:
                        alias_narrowed = alias_narrowed.narrow_by_runtime_class(
                            cls, False
                        )
                if (
                    alias_narrowed is not BOTTOM
                    and alias_narrowed is not alias_current.type
                ):
                    state[alias] = visitor.narrowed_fact(alias_current, alias_narrowed)
    return True


# ``isinstance`` calls with the classes
# separated at the call site: ``(x, A, B, ...)``. The tuple is never built.
ISINSTANCE = defknown(
    "%isinstance",
    "call",
    "unsafe",
    derive_type=_isinstance_answer,
    constrain_branch=_constrain_instance_check,
)
ISINSTANCE_BUILTIN = defknown("isinstance", "call", "unsafe")
SETATTR = defknown(
    "%setattr", "call", "unsafe", result=False, non_integer_first_arg=True
)
RESOLVE_METHOD = defknown("%resolve_method", "call", non_integer_first_arg=True)
RESOLVE_SUPER_METHOD = defknown("%resolve_super_method", "call")
CALL_RESOLVED = defknown("%call_resolved", "call", "unsafe")

# Definitions executed when a module runs.
MAKE_FUNCTION = defknown("%make_function", "flushable")
MAKE_CLASS = defknown("%make_class", "call", "unsafe")
# A class body's namespace: creation, and reads, stores and deletions of the
# names the body binds. Reads fall back to the module globals and builtins.
CLASS_NAMESPACE = defknown("%class_namespace", "call", "unsafe")
CLASS_LOAD = defknown("%class_load", "call")
CLASS_LOAD_OR = defknown("%class_load_or", "call")
CLASS_STORE = defknown("%class_store", "unsafe", result=False)
CLASS_DELETE = defknown("%class_delete", "unsafe", result=False)
IMPORT = defknown("%import", "call", "unsafe")
# ``from module import *``: bind the module's public names in the globals.
IMPORT_STAR = defknown("%import_star", "unsafe", result=False)
HAS_BINDING = defknown("%has_binding", derive_type=lambda node: BOOLEAN)
# The globals namespace of the function being compiled: every native entry
# receives it, and ``globals()`` returns it (converted where it is called).
CURRENT_GLOBALS = defknown("%current_globals", "flushable")

# Suspend the running generator, returning the value it is resumed with.
YIELD = defknown("%yield", "call", "unsafe")

# Displays and formatting.
BUILD_LIST = defknown("%build_list", "flushable", derive_type=_derive_build_list)
BUILD_TUPLE = defknown(
    "%build_tuple",
    "flushable",
    "foldable",
    derive_type=_derive_build_tuple,
)
UNPACK_REF = defknown(
    "%unpack_ref",
    derive_type=lambda node: (
        None
        if _DERIVATION_MODE.active
        else _arg_type(node.args[0]).getitem_type(_arg_type(node.args[1]))
    ),
    borrowed_args=frozenset({0, 1}),
)
FORMAT = defknown("%format", "call", derive_type=lambda node: TEXT_TYPE)

# Guarded counted iteration over a builtin range.
RANGE_PREPARE = defknown(
    "%range_prepare",
    derive_type=lambda node: None if _DERIVATION_MODE.active else BOOLEAN,
    numeric=True,
    non_integer_first_arg=True,
    borrowed_args=frozenset({0, 1, 2}),
)
RANGE_AVAILABLE = defknown(
    "%range_available",
    "flushable",
    derive_type=lambda node: BOOLEAN,
    borrowed_args=frozenset({0}),
)
RANGE_ADVANCE = defknown(
    "%range_advance", "unsafe", numeric=True, borrowed_args=frozenset({0})
)
SEQUENCE_AVAILABLE = defknown(
    "%sequence_available",
    "unsafe",
    derive_type=lambda node: None if _DERIVATION_MODE.active else BOOLEAN,
    borrowed_args=frozenset({0, 1}),
)
GETSLICE = defknown(
    "%getslice",
    "call",
    "unsafe",
    derive_type=_derive_getslice,
    numeric=True,
    non_integer_first_arg=True,
)
LEN = defknown(
    "len",
    "call",
    "unsafe",
    derive_type=lambda node: IntegerType(0, (1 << 60) - 1),
    non_integer_first_arg=True,
)


def derive_min2_types(left: ValueType, right: ValueType) -> ValueType:
    if left is BOTTOM or right is BOTTOM:
        return BOTTOM
    if left.fits(PYTHON_INTEGER) and right.fits(PYTHON_INTEGER):
        lo = (
            min(left.minimum, right.minimum)
            if None not in (left.minimum, right.minimum)
            else None
        )
        hi = (
            min(v for v in (left.maximum, right.maximum) if v is not None)
            if (left.maximum is not None or right.maximum is not None)
            else None
        )
        left_can_be_bool = not left.fits(INTEGER) and (
            right.maximum is None or right.maximum >= 0
        )
        right_can_be_bool = not right.fits(INTEGER) and (
            left.maximum is None or left.maximum > 0
        )
        if not left_can_be_bool and not right_can_be_bool:
            return IntegerType(lo, hi)
        return PythonIntegerType(lo, hi)
    return left.join(right)


def derive_max2_types(left: ValueType, right: ValueType) -> ValueType:
    if left is BOTTOM or right is BOTTOM:
        return BOTTOM
    if left.fits(PYTHON_INTEGER) and right.fits(PYTHON_INTEGER):
        lo = (
            max(v for v in (left.minimum, right.minimum) if v is not None)
            if (left.minimum is not None or right.minimum is not None)
            else None
        )
        hi = (
            max(left.maximum, right.maximum)
            if None not in (left.maximum, right.maximum)
            else None
        )
        left_can_be_bool = not left.fits(INTEGER) and (
            right.minimum is None or right.minimum <= 1
        )
        right_can_be_bool = not right.fits(INTEGER) and (
            left.minimum is None or left.minimum < 1
        )
        if not left_can_be_bool and not right_can_be_bool:
            return IntegerType(lo, hi)
        return PythonIntegerType(lo, hi)
    return left.join(right)


MIN2 = defknown(
    "%min2",
    "foldable",
    "flushable",
    derive_type=lambda node: (
        None
        if _DERIVATION_MODE.active or len(node.args) != 2
        else derive_min2_types(_arg_type(node.args[0]), _arg_type(node.args[1]))
    ),
    numeric=True,
)
MAX2 = defknown(
    "%max2",
    "foldable",
    "flushable",
    derive_type=lambda node: (
        None
        if _DERIVATION_MODE.active or len(node.args) != 2
        else derive_max2_types(_arg_type(node.args[0]), _arg_type(node.args[1]))
    ),
    numeric=True,
)
MIN_BUILTIN = defknown("min", "call", "unsafe")
MAX_BUILTIN = defknown("max", "call", "unsafe")
MATH_SQRT = defknown(
    "%math_sqrt",
    derive_type=lambda node: FLOAT,
    numeric=True,
)
MATH_SQRT_IMPORT = defknown(
    "math.sqrt",
    "call",
    "unsafe",
    derive_type=lambda node: FLOAT,
    numeric=True,
)

_PROTOCOL_DERIVE_TYPES = {
    "getitem": lambda node: (
        None
        if _DERIVATION_MODE.active
        else _arg_type(node.args[0]).getitem_type(_arg_type(node.args[1]))
    ),
    "setitem": lambda node: NONE_TYPE,
    "unpack": _derive_unpack,
}

_NON_INTEGER_FIRST_PROTOCOLS = frozenset(
    {
        "getitem",
        "setitem",
        "delitem",
        "delete_attribute",
        "iterate",
        "next_item",
        "unpack",
    }
)
_INDEX_SECOND_PROTOCOLS = frozenset({"getitem", "setitem", "delitem"})

# Compiled stdlib protocols, keyed by the name the sequence module exports.
PROTOCOLS = {
    name: defknown(
        f"%{name}",
        "call",
        "unsafe",
        protocol=name,
        derive_type=_PROTOCOL_DERIVE_TYPES.get(name),
        numeric=(name == "make_slice"),
        non_integer_first_arg=(name in _NON_INTEGER_FIRST_PROTOCOLS),
        index_second_arg=(name in _INDEX_SECOND_PROTOCOLS),
    )
    for name in (
        "iterate",
        "next_item",
        "getitem",
        "setitem",
        "unpack",
        "unpack_starred",
        "exception_matches",
        "iteration_exhausted",
        "context_exit_method",
        "context_enter",
        "context_exit",
        "cell_new",
        "cell_get",
        "cell_get_optional",
        "cell_unbound",
        "cell_set",
        "cell_clear",
        "snapshot_locals",
        "call_with_locals",
        "call_with_eval_context",
        "globals_dict",
        "except_star_start",
        "except_star_match",
        "except_star_matched",
        "except_star_caught",
        "except_star_finish",
        "exception_instance",
        "set_exception_cause",
        "ellipsis_value",
        "make_slice",
        "delitem",
        "delete_attribute",
        "append_argument",
        "extend_arguments",
        "add_keyword",
        "merge_keywords",
        "call_expanded",
        "call_expanded_positional",
        "tuple_from_list",
        "dict_merge",
        "build_template",
        "make_interpolation",
        "make_generator",
        "yield_from",
        "make_coroutine",
        "make_async_generator",
        "await_value",
        "async_generator_yield",
        "build_dict",
        "build_set",
    )
}

# Exception state.
CATCH_EXCEPTION = defknown("%catch_exception", "unsafe")
ENTER_HANDLER = defknown("%enter_handler", "unsafe", result=False)
LEAVE_HANDLER = defknown("%leave_handler", "unsafe", result=False)
ASSERTION_FAILURE = defknown("%assertion_failure", "unsafe", result=False)
RAISE = defknown("%raise", "unsafe", "never_returns", result=False)
RERAISE = defknown("%reraise", "unsafe", "never_returns", result=False)
RESUME_EXCEPTION = defknown(
    "%resume_exception", "unsafe", "never_returns", result=False
)
UNWIND = defknown("%unwind", "unsafe", "never_returns", result=False)

__all__ = [
    "ASSERTION_FAILURE",
    "BINARY",
    "BUILD_LIST",
    "BUILD_TUPLE",
    "CALL_RESOLVED",
    "CATCH_EXCEPTION",
    "CLASS_DELETE",
    "CLASS_LOAD",
    "CLASS_LOAD_OR",
    "CLASS_NAMESPACE",
    "CLASS_STORE",
    "COMPARE",
    "CURRENT_GLOBALS",
    "ENTER_HANDLER",
    "FORMAT",
    "GETATTR",
    "GETATTR_DEFAULT",
    "GETSLICE",
    "HASATTR",
    "HAS_BINDING",
    "IMPORT",
    "IMPORT_STAR",
    "INPLACE_BINARY",
    "LEAVE_HANDLER",
    "LEN",
    "MAKE_CLASS",
    "MAKE_FUNCTION",
    "MATH_SQRT",
    "MATH_SQRT_IMPORT",
    "MAX2",
    "MAX_BUILTIN",
    "MIN2",
    "MIN_BUILTIN",
    "PROTOCOLS",
    "RAISE",
    "RANGE_ADVANCE",
    "RANGE_AVAILABLE",
    "RANGE_PREPARE",
    "RERAISE",
    "RESOLVE_METHOD",
    "RESOLVE_SUPER_METHOD",
    "RESUME_EXCEPTION",
    "SEQUENCE_AVAILABLE",
    "SETATTR",
    "UNARY",
    "UNPACK_REF",
    "UNWIND",
    "YIELD",
    "flow_propagation",
    "fun_info",
]
