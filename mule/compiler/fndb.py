"""The known-function database for Python operations, following ``fndb.lisp``.

Names beginning with ``%`` are compiler-internal operations that IR1
conversion emits for Python syntax and for Mule's runtime protocols. Their
attributes are deliberately conservative: nearly every Python operator can run
user code and raise, so almost nothing is flushable or foldable until the
optimizer proves operand types.
"""

from __future__ import annotations

from mule import ast

from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    DICT_TYPE,
    EMPTY_LIST_TYPE,
    EMPTY_SET_TYPE,
    FLOAT,
    FUNCTION_TYPE,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    SET_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    PythonIntegerType,
    TextType,
    TupleType,
    ValueType,
)
from .knownfun import defknown, fun_info
from .node import CallKind, LeafVisitor, Lvar, Node, NodeVisitor

TUPLE_TYPE = TupleType()
SMALL_INTS = {i: IntegerType(i, i) for i in range(-16, 257)}
SMALL_TUPLES = tuple(TupleType(i) for i in range(16))
INTERNED_TEXTS = {
    "": TextType(""),
    **{chr(code): TextType(chr(code)) for code in range(256)},
}


class LiteralType(ast.Visitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_NoneLiteral(self, node):
        return NONE_TYPE

    def visit_BooleanLiteral(self, node):
        return BOOLEAN_TRUE if node.value else BOOLEAN_FALSE

    def visit_IntegerLiteral(self, node):
        value = node.value
        cached = SMALL_INTS.get(value)
        return IntegerType(value, value) if cached is None else cached

    def visit_FloatLiteral(self, node):
        return FLOAT

    def visit_TextLiteral(self, node):
        return INTERNED_TEXTS.get(node.value, TEXT_TYPE)

    def visit_BytesLiteral(self, node):
        return BYTES_TYPE

    def visit_Tuple(self, node):
        arity = len(node.elements)
        if 0 < arity <= 64:
            elements = tuple(el.accept(self) for el in node.elements)
            if not all(el is UNKNOWN for el in elements):
                return TupleType(arity, elements)
        return SMALL_TUPLES[arity] if arity < 16 else TupleType(arity)


LITERAL_TYPE = LiteralType()


def constant_type(literal) -> ValueType:
    return literal.accept(LITERAL_TYPE)


class DeriveNodeType(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_Ref(self, node):
        derived = node.leaf.accept(self)
        if node.derived_type is not None and (
            derived is UNKNOWN
            or (
                node.derived_type.as_bound().fits(derived.as_bound())
                if derived.may_be_unbound
                else node.derived_type.fits(derived)
            )
        ):
            return node.derived_type
        return derived

    def visit_Leaf(self, leaf):
        return leaf.type if leaf.type is not None else UNKNOWN

    def visit_Constant(self, leaf):
        return constant_type(leaf.value)

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
        return node.contract.refine(lvar_type(node.value))

    def visit_Combination(self, node):
        derived = UNKNOWN
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.derive_type is not None
        ):
            result = node.fun_info.derive_type(node)
            if result is not None:
                derived = result
        if node.derived_type is not None and (
            derived is UNKNOWN
            or (
                node.derived_type.as_bound().fits(derived.as_bound())
                if derived.may_be_unbound
                else node.derived_type.fits(derived)
            )
        ):
            return node.derived_type
        return derived

    def visit_Exit(self, node):
        return lvar_type(node.value) if node.value is not None else UNKNOWN


DERIVE_NODE_TYPE = DeriveNodeType()


class NodeDerivedType(NodeVisitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_ValuedNode(self, node):
        if node.derived_type is None:
            node.derived_type = node.accept(DERIVE_NODE_TYPE)
        return node.derived_type


NODE_DERIVED_TYPE = NodeDerivedType()


def node_derived_type(node: Node) -> ValueType:
    """The derived value type of ``node``, computed and cached on demand."""
    return node.accept(NODE_DERIVED_TYPE)


def lvar_type(lvar: Lvar) -> ValueType:
    """The union of the derived types of ``lvar``'s producing nodes."""
    if lvar.derived_type is not None:
        return lvar.derived_type
    if not lvar.uses:
        return BOTTOM
    derived = node_derived_type(lvar.uses[0])
    for use in lvar.uses[1:]:
        derived = derived.join(node_derived_type(use))
    lvar.derived_type = derived
    return derived


def derive_getslice(node) -> ValueType | None:
    if any(lvar_type(arg).may_be_unbound for arg in node.args):
        return UNKNOWN
    arg_types = [lvar_type(arg) for arg in node.args[1:4]]
    return lvar_type(node.args[0]).getslice_type(*arg_types)


def derive_build_list(node) -> ValueType | None:
    result: ValueType = EMPTY_LIST_TYPE
    for arg in node.args:
        result = result.with_element(lvar_type(arg))
    return result


def derive_build_tuple(node) -> ValueType | None:
    arity = len(node.args)
    if 0 < arity <= 64:
        elements = tuple(lvar_type(arg) for arg in node.args)
        if not all(el is UNKNOWN for el in elements):
            return TupleType(arity, elements)
    return TupleType(arity)


def derive_unpack(node) -> ValueType | None:
    arity = lvar_type(node.args[1]).exact_integer()
    if arity is not None and arity >= 0:
        seq_type = lvar_type(node.args[0])
        if seq_type.fits_tuple(TupleType(arity)):
            return seq_type
        return TupleType(arity)
    return TupleType(None)


def is_builtin_sequence_type(t: ValueType) -> bool:
    return (
        t.fits_text(TEXT_TYPE)
        or t.fits(BYTES_TYPE)
        or t.fits(BYTEARRAY_TYPE)
        or t.fits(LIST_TYPE)
        or t.fits_tuple(TUPLE_TYPE)
    )


def is_bounded_repeat_count(t: ValueType, max_bound: int = 1 << 30) -> bool:
    return (
        t.fits(PYTHON_INTEGER)
        and t.minimum is not None
        and t.maximum is not None
        and -(1 << 63) <= t.minimum
        and t.maximum <= max_bound
    )


def flushable_add(node) -> bool:
    if len(node.args) != 2:
        return False
    left, right = lvar_type(node.args[0]), lvar_type(node.args[1])
    if left.may_be_unbound or right.may_be_unbound:
        return False
    return (
        (left.fits(REAL) and right.fits(REAL))
        or (left.fits_text(TEXT_TYPE) and right.fits_text(TEXT_TYPE))
        or (
            (left.fits(BYTES_TYPE) or left.fits(BYTEARRAY_TYPE))
            and (right.fits(BYTES_TYPE) or right.fits(BYTEARRAY_TYPE))
        )
        or (left.fits(LIST_TYPE) and right.fits(LIST_TYPE))
        or (left.fits_tuple(TUPLE_TYPE) and right.fits_tuple(TUPLE_TYPE))
    )


def flushable_subtract(node) -> bool:
    if len(node.args) != 2:
        return False
    left, right = lvar_type(node.args[0]), lvar_type(node.args[1])
    return (
        not left.may_be_unbound
        and not right.may_be_unbound
        and left.fits(REAL)
        and right.fits(REAL)
    )


def flushable_multiply(node) -> bool:
    if len(node.args) != 2:
        return False
    left, right = lvar_type(node.args[0]), lvar_type(node.args[1])
    if left.may_be_unbound or right.may_be_unbound:
        return False
    if left.fits(REAL) and right.fits(REAL):
        return True
    for seq_t, cnt_t in ((left, right), (right, left)):
        if is_builtin_sequence_type(seq_t) and is_bounded_repeat_count(cnt_t):
            return True
    return False


def flushable_getslice(node) -> bool:
    if len(node.args) != 4:
        return False
    seq_t, start_t, stop_t, step_t = (lvar_type(arg) for arg in node.args)
    if (
        seq_t.may_be_unbound
        or start_t.may_be_unbound
        or stop_t.may_be_unbound
        or step_t.may_be_unbound
    ):
        return False
    return (
        is_builtin_sequence_type(seq_t)
        and (start_t.fits(PYTHON_INTEGER) or start_t.fits_none(NONE_TYPE))
        and (stop_t.fits(PYTHON_INTEGER) or stop_t.fits_none(NONE_TYPE))
        and (
            step_t.fits_none(NONE_TYPE)
            or (
                step_t.fits(PYTHON_INTEGER)
                and (
                    (step_t.minimum is not None and step_t.minimum > 0)
                    or (step_t.maximum is not None and step_t.maximum < 0)
                )
            )
        )
    )


def flushable_compare(node, op: ast.ComparisonOperator) -> bool:
    if len(node.args) != 2:
        return False
    left, right = lvar_type(node.args[0]), lvar_type(node.args[1])
    if left.may_be_unbound or right.may_be_unbound:
        return False
    if op in (ast.ComparisonOperator.IS, ast.ComparisonOperator.IS_NOT):
        return True
    if op in (ast.ComparisonOperator.IN, ast.ComparisonOperator.NOT_IN):
        return False
    return (
        (left.fits(REAL) and right.fits(REAL))
        or (left.fits_text(TEXT_TYPE) and right.fits_text(TEXT_TYPE))
        or (
            (left.fits(BYTES_TYPE) or left.fits(BYTEARRAY_TYPE))
            and (right.fits(BYTES_TYPE) or right.fits(BYTEARRAY_TYPE))
        )
    )


BINARY_FLUSHABLE_TESTS = {
    ast.BinaryOperator.ADD: flushable_add,
    ast.BinaryOperator.SUBTRACT: flushable_subtract,
    ast.BinaryOperator.MULTIPLY: flushable_multiply,
}

# Binary operators: one known per operator and one per in-place form.
BINARY = {
    operator: defknown(
        f"%{operator.name.lower()}",
        "call",
        derive_type=lambda node, op=operator: lvar_type(node.args[0]).binary(
            op, lvar_type(node.args[1])
        ),
        flushable_test=BINARY_FLUSHABLE_TESTS.get(operator),
        numeric=True,
    )
    for operator in ast.BinaryOperator
}
INPLACE_BINARY = {
    operator: defknown(
        f"%i{operator.name.lower()}",
        "call",
        "unsafe",
        derive_type=lambda node, op=operator: lvar_type(node.args[0]).binary(
            op, lvar_type(node.args[1])
        ),
        numeric=True,
    )
    for operator in ast.BinaryOperator
}
UNARY = {
    operator: defknown(
        f"%unary_{operator.name.lower()}",
        "call",
        derive_type=lambda node, op=operator: lvar_type(node.args[0]).unary(op),
        numeric=(operator is not ast.UnaryOperator.NOT),
    )
    for operator in ast.UnaryOperator
}
COMPARE = {
    operator: defknown(
        f"%compare_{operator.name.lower()}",
        "call",
        derive_type=lambda node, op=operator: lvar_type(node.args[0]).comparison(
            op, lvar_type(node.args[1])
        ),
        flushable_test=lambda node, op=operator: flushable_compare(node, op),
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
# ``type`` itself, so a one-argument call to an unmutated global ``type``
# reaches the class-of transform in ``protocols``. Class creation
# (``type(name, bases, namespace)``) stays a generic call.
TYPE_BUILTIN = defknown("type", "call", "unsafe")


def isinstance_answer(node) -> ValueType | None:
    """``True`` when the operand's class is proven to be one of the classes
    or a subclass. ``False`` is never derived: a metaclass ``__instancecheck__``
    (``abc.ABCMeta`` with registered virtual subclasses) can accept an
    instance whose class is unrelated, and the compiler cannot see that."""
    value, *classes = (lvar_type(arg) for arg in node.args)
    if any(value.is_instance_of(cls) is True for cls in classes):
        return BOOLEAN_TRUE
    return BOOLEAN


def constrain_instance_check(visitor, node, state, truth: bool) -> bool:
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
    return True


# ``isinstance`` calls with the classes
# separated at the call site: ``(x, A, B, ...)``. The tuple is never built.
ISINSTANCE = defknown(
    "%isinstance",
    "call",
    "unsafe",
    derive_type=isinstance_answer,
    constrain_branch=constrain_instance_check,
)
ISINSTANCE_BUILTIN = defknown("isinstance", "call", "unsafe")
SETATTR = defknown(
    "%setattr", "call", "unsafe", result=False, non_integer_first_arg=True
)
PHYSICAL_FIELD_REF = defknown(
    "%physical_field_ref", borrowed_args=frozenset({0}), non_integer_first_arg=True
)
PHYSICAL_FIELD_SET = defknown(
    "%physical_field_set",
    "call",
    "unsafe",
    derive_type=lambda node: NONE_TYPE,
    borrowed_args=frozenset({0}),
    non_integer_first_arg=True,
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
HAS_BINDING = defknown(
    "%has_binding", derive_type=lambda node: BOOLEAN, borrowed_args=frozenset({0})
)
# The globals namespace of the function being compiled: every native entry
# receives it, and ``globals()`` returns it (converted where it is called).
CURRENT_GLOBALS = defknown("%current_globals", "flushable")

# Suspend the running generator, returning the value it is resumed with.
YIELD = defknown("%yield", "call", "unsafe")

# Displays and formatting.
BUILD_LIST = defknown("%build_list", "flushable", derive_type=derive_build_list)
BUILD_TUPLE = defknown(
    "%build_tuple",
    "flushable",
    "foldable",
    derive_type=derive_build_tuple,
)
UNPACK_REF = defknown(
    "%unpack_ref",
    derive_type=lambda node: lvar_type(node.args[0]).getitem_type(
        lvar_type(node.args[1])
    ),
    borrowed_args=frozenset({0, 1}),
)
FORMAT = defknown("%format", "call", derive_type=lambda node: TEXT_TYPE)

# Guarded counted iteration over a builtin range.
RANGE_PREPARE = defknown(
    "%range_prepare",
    derive_type=lambda node: BOOLEAN,
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
    derive_type=lambda node: BOOLEAN,
    borrowed_args=frozenset({0, 1}),
)
GETSLICE = defknown(
    "%getslice",
    "call",
    "unsafe",
    derive_type=derive_getslice,
    flushable_test=flushable_getslice,
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
    return left.join(right).widen_constant()


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
    return left.join(right).widen_constant()


def derive_abs_type(value: ValueType) -> ValueType:
    if value is BOTTOM:
        return BOTTOM
    if value.fits(PYTHON_INTEGER) and not value.may_be_unbound:
        lo, hi = value.minimum, value.maximum
        if lo is not None and lo >= 0:
            return IntegerType(lo, hi)
        if hi is not None and hi <= 0:
            return IntegerType(-hi, -lo if lo is not None else None)
        res_hi = max(-lo, hi) if (lo is not None and hi is not None) else None
        return IntegerType(0, res_hi)
    if value is FLOAT:
        return FLOAT
    return UNKNOWN


MIN2 = defknown(
    "%min2",
    "foldable",
    "flushable",
    derive_type=lambda node: (
        None
        if len(node.args) != 2
        else derive_min2_types(lvar_type(node.args[0]), lvar_type(node.args[1]))
    ),
    numeric=True,
)
MAX2 = defknown(
    "%max2",
    "foldable",
    "flushable",
    derive_type=lambda node: (
        None
        if len(node.args) != 2
        else derive_max2_types(lvar_type(node.args[0]), lvar_type(node.args[1]))
    ),
    numeric=True,
)
MIN_BUILTIN = defknown("min", "call", "unsafe")
MAX_BUILTIN = defknown("max", "call", "unsafe")
INT_BUILTIN = defknown("int", "call", "unsafe")
ORD_BUILTIN = defknown("ord", "call", "unsafe")
SET_BUILTIN = defknown("set", "call", "unsafe")
PARSE_INT_TEXT = defknown(
    "%parse_int_text",
    "unsafe",
    derive_type=lambda node: INTEGER,
    non_integer_first_arg=True,
)
PARSE_INT_SLICE = defknown(
    "%parse_int_slice",
    "unsafe",
    derive_type=lambda node: INTEGER,
    non_integer_first_arg=True,
)
TEXT_SLICE_COMPARE = defknown(
    "%text_slice_compare",
    "flushable",
    derive_type=lambda node: BOOLEAN,
    non_integer_first_arg=True,
)
TEXT_SLICE2_COMPARE = defknown(
    "%text_slice2_compare",
    "flushable",
    derive_type=lambda node: BOOLEAN,
    non_integer_first_arg=True,
)
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


def derive_list_fill(node) -> ValueType | None:
    return EMPTY_LIST_TYPE.with_element(lvar_type(node.args[0]))


def propagate_list_fill(visitor, node) -> ValueType:
    return EMPTY_LIST_TYPE.with_element(visitor.type_of(node.args[0]))


PROTOCOL_DERIVE_TYPES = {
    "_list_fill": derive_list_fill,
    "getitem": lambda node: lvar_type(node.args[0]).getitem_type(
        lvar_type(node.args[1])
    ),
    "setitem": lambda node: NONE_TYPE,
    "dict_setitem": lambda node: NONE_TYPE,
    "dict_len": lambda node: IntegerType(0, (1 << 60) - 1),
    "table_len": lambda node: IntegerType(0, (1 << 60) - 1),
    "empty_set": lambda node: EMPTY_SET_TYPE,
    "set_add": lambda node: NONE_TYPE,
    "set_discard": lambda node: NONE_TYPE,
    "set_remove": lambda node: NONE_TYPE,
    "set_and": lambda node: lvar_type(node.args[0]).binary(
        "&", lvar_type(node.args[1])
    ),
    "set_or": lambda node: lvar_type(node.args[0]).binary("|", lvar_type(node.args[1])),
    "set_sub": lambda node: lvar_type(node.args[0]).binary(
        "-", lvar_type(node.args[1])
    ),
    "set_xor": lambda node: lvar_type(node.args[0]).binary(
        "^", lvar_type(node.args[1])
    ),
    "unpack": derive_unpack,
}

NON_INTEGER_FIRST_PROTOCOLS = frozenset(
    {
        "getitem",
        "setitem",
        "delitem",
        "delete_attribute",
        "iterate",
        "next_item",
        "unpack",
        "dict_len",
        "table_len",
        "set_add",
        "set_discard",
        "set_remove",
        "set_and",
        "set_or",
        "set_sub",
        "set_xor",
    }
)
INDEX_SECOND_PROTOCOLS = frozenset({"getitem", "setitem", "delitem"})

PROTOCOL_ARGUMENT_TYPES = {
    "dict_getitem": (DICT_TYPE, UNKNOWN),
    "dict_setitem": (DICT_TYPE, UNKNOWN, UNKNOWN),
    "dict_len": (DICT_TYPE,),
    "table_len": (SET_TYPE,),
    "set_add": (SET_TYPE, UNKNOWN),
    "set_discard": (SET_TYPE, UNKNOWN),
    "set_remove": (SET_TYPE, UNKNOWN),
    "set_and": (SET_TYPE, SET_TYPE),
    "set_or": (SET_TYPE, SET_TYPE),
    "set_sub": (SET_TYPE, SET_TYPE),
    "set_xor": (SET_TYPE, SET_TYPE),
}


def flushable_list_fill(node) -> bool:
    if len(node.args) != 2:
        return False
    elem_t, cnt_t = lvar_type(node.args[0]), lvar_type(node.args[1])
    return (
        not elem_t.may_be_unbound
        and not cnt_t.may_be_unbound
        and is_bounded_repeat_count(cnt_t, (1 << 60) - 1)
    )


def flushable_make_slice(node) -> bool:
    return all(not lvar_type(arg).may_be_unbound for arg in node.args)


PROTOCOL_FLUSHABLE_TESTS = {
    "_list_fill": flushable_list_fill,
    "make_slice": flushable_make_slice,
    "empty_set": lambda node: not node.args,
    "dict_len": lambda node: (
        len(node.args) == 1 and not lvar_type(node.args[0]).may_be_unbound
    ),
    "table_len": lambda node: (
        len(node.args) == 1 and not lvar_type(node.args[0]).may_be_unbound
    ),
}

# Compiled stdlib protocols, keyed by the name the sequence module exports.
PROTOCOLS = {
    name: defknown(
        f"%{name}",
        "call",
        "unsafe",
        protocol=name,
        derive_type=PROTOCOL_DERIVE_TYPES.get(name),
        flushable_test=PROTOCOL_FLUSHABLE_TESTS.get(name),
        propagate=propagate_list_fill if name == "_list_fill" else None,
        numeric=(name == "make_slice"),
        non_integer_first_arg=(name in NON_INTEGER_FIRST_PROTOCOLS),
        index_second_arg=(name in INDEX_SECOND_PROTOCOLS),
        argument_types=PROTOCOL_ARGUMENT_TYPES.get(name),
    )
    for name in (
        "_list_fill",
        "dict_getitem",
        "dict_setitem",
        "dict_len",
        "table_len",
        "empty_set",
        "set_add",
        "set_discard",
        "set_remove",
        "set_and",
        "set_or",
        "set_sub",
        "set_xor",
        "iterate",
        "next_item",
        "getitem",
        "setitem",
        "unpack",
        "unpack_starred",
        "exception_matches",
        "iteration_exhausted",
        "generator_exception",
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
    "DERIVE_NODE_TYPE",
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
    "INTERNED_TEXTS",
    "INT_BUILTIN",
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
    "ORD_BUILTIN",
    "PARSE_INT_SLICE",
    "PARSE_INT_TEXT",
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
    "SMALL_INTS",
    "SMALL_TUPLES",
    "TEXT_SLICE2_COMPARE",
    "TEXT_SLICE_COMPARE",
    "UNARY",
    "UNPACK_REF",
    "UNWIND",
    "YIELD",
    "constant_type",
    "fun_info",
    "lvar_type",
    "node_derived_type",
]
