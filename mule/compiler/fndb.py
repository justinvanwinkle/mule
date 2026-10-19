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
    LIST_TYPE,
    NONE_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    TextType,
    TupleType,
    ValueType,
)
from .knownfun import defknown, fun_info
from .node import CallKind, GlobalVar, LeafVisitor, NodeVisitor, Ref


class _DerivationMode(local):
    def __init__(self):
        self.active = False


_DERIVATION_MODE = _DerivationMode()
_PROTOCOL_PREFIXES = (
    "mule_sequences_",
    "mule_format_",
    "mule_arguments_",
    "mule_native_class_",
    "mule_print_",
)


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


class _UnmutatedIntRef(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return False

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf.name == "int" and not leaf.sets


_UNMUTATED_INT_REF = _UnmutatedIntRef()


def derive_builtin_call_type(node) -> ValueType:
    """Return ``INTEGER`` for an unmutated ``int(...)`` call outside flow propagation."""
    if _DERIVATION_MODE.active:
        return UNKNOWN
    if node.kind not in (CallKind.FULL, CallKind.GUARDED):
        return UNKNOWN
    if node.info is not None and node.info.keywords:
        return UNKNOWN
    if len(node.args) > 2 or node.fun is None or len(node.fun.uses) != 1:
        return UNKNOWN
    lambda_ = node.home_lambda
    if lambda_ is not None and (
        lambda_.policy.safety >= 3 or lambda_.name.startswith(_PROTOCOL_PREFIXES)
    ):
        return UNKNOWN
    if node.fun.use().accept(_UNMUTATED_INT_REF):
        return INTEGER
    return UNKNOWN


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
            TextType(node.value) if len(node.value.encode("utf-8")) <= 64 else TEXT_TYPE
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
        return derive_builtin_call_type(node)

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
    receiver_type = _arg_type(node.args[0])
    if any(_arg_type(arg).may_be_unbound for arg in node.args):
        return UNKNOWN
    if receiver_type.fits_text(TEXT_TYPE):
        return TEXT_TYPE
    if receiver_type is BYTES_TYPE:
        return BYTES_TYPE
    if receiver_type.fits_list(LIST_TYPE):
        return receiver_type
    return UNKNOWN


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
    )
    for operator in ast.ComparisonOperator
}

# Attribute and method protocols.
GETATTR = defknown("%getattr", "call")
SETATTR = defknown("%setattr", "call", "unsafe", result=False)
RESOLVE_METHOD = defknown("%resolve_method", "call")
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
)
FORMAT = defknown("%format", "call", derive_type=lambda node: TEXT_TYPE)

# Guarded counted iteration over a builtin range.
RANGE_PREPARE = defknown(
    "%range_prepare",
    derive_type=lambda node: None if _DERIVATION_MODE.active else BOOLEAN,
)
RANGE_AVAILABLE = defknown(
    "%range_available", "flushable", derive_type=lambda node: BOOLEAN
)
RANGE_ADVANCE = defknown("%range_advance", "unsafe")
SEQUENCE_AVAILABLE = defknown(
    "%sequence_available",
    "unsafe",
    derive_type=lambda node: None if _DERIVATION_MODE.active else BOOLEAN,
)
GETSLICE = defknown("%getslice", "call", "unsafe", derive_type=_derive_getslice)
LEN = defknown("len", "call", "unsafe")

_PROTOCOL_DERIVE_TYPES = {
    "getitem": lambda node: (
        None
        if _DERIVATION_MODE.active
        else _arg_type(node.args[0]).getitem_type(_arg_type(node.args[1]))
    ),
    "setitem": lambda node: NONE_TYPE,
    "unpack": _derive_unpack,
}

# Compiled stdlib protocols, keyed by the name the sequence module exports.
PROTOCOLS = {
    name: defknown(
        f"%{name}",
        "call",
        "unsafe",
        protocol=name,
        derive_type=_PROTOCOL_DERIVE_TYPES.get(name),
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
        "context_exit",
        "cell_new",
        "cell_get",
        "cell_get_optional",
        "cell_set",
        "cell_clear",
        "snapshot_locals",
        "call_with_locals",
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
    "GETSLICE",
    "HAS_BINDING",
    "IMPORT",
    "IMPORT_STAR",
    "INPLACE_BINARY",
    "LEAVE_HANDLER",
    "LEN",
    "MAKE_CLASS",
    "MAKE_FUNCTION",
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
