"""Compiled Mule language protocols and the physical primitives they bind.

The compiled stdlib modules (``stdlib/_sequences.py``, ``_format.py``,
``_arguments.py``, ``_native_class.py``, ``_print.py``) are ordinary Python
compiled through the same converter and IR1 phases as user code. Their
underscored primitive names resolve to known functions whose templates emit
the runtime's physical operations; nothing else distinguishes them from user
modules. Each protocol module's ``prepare`` appends its lambdas and facts to
the program being generated.
"""

from __future__ import annotations

from mule import ast

from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BYTES_TYPE,
    CLASS_STR_TYPE,
    CLASS_TUPLE_TYPE,
    EMPTY_LIST_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    INTEGER,
    INTERNED_TEXT_TYPE,
    OPTIONAL_FIXNUM,
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    BooleanType,
    IntegerType,
    TextType,
    TupleType,
    UniqueObjectType,
    ValueType,
)
from .fndb import _DERIVATION_MODE, _arg_type, defknown
from .ir1opt import bound_constant_literal
from .knownfun import defconstrain, defpropagate, deftransform
from .main import compile_toplevel, ir1_phases
from .node import ForeignCallTarget
from .vops import (
    allocate,
    compiled_call,
    initialize,
    invalid_initializer,
    lookup_method,
    numeric_modulo,
    physical_field_read,
    physical_field_write,
    primitive,
    read_int64,
    render_text,
    text_access,
    text_result,
    write_int64,
)

_LOOKUPS = {
    "_pos_method": "__pos__",
    "_neg_method": "__neg__",
    "_invert_method": "__invert__",
    "_abs_method": "__abs__",
    "_getitem_method": "__getitem__",
    "_setitem_method": "__setitem__",
    "_delitem_method": "__delitem__",
    "_reversed_method": "__reversed__",
    "_iter_method": "__iter__",
    "_next_method": "__next__",
    "_getattr_method": "__getattr__",
    "_index_method": "__index__",
    "_init_method": "__init__",
    "_str_method": "__str__",
    "_repr_method": "__repr__",
    "_format_method": "__format__",
}
_TEXT_ACCESS = {
    name: "mule" + name
    for name in (
        "_storage_length",
        "_storage_reserve",
        "_storage_read",
        "_storage_write",
        "_storage_append",
        "_storage_insert",
        "_storage_delete",
        "_storage_tuple",
        "_tuple_storage",
        "_storage_compatible",
        "_integer_storage",
        "_text_storage",
        "_text_length",
        "_text_character",
        "_text_codepoint",
        "_text_slice",
        "_text_join",
        "_text_from_codepoints",
        "_storage_hash",
        "_identity_hash",
        "_type_of",
        "_type_name",
        "_class_identity",
        "_class_parent",
        "_class_is_subclass",
        "_is_class",
        "_is_instance",
        "_class_lookup_default",
        "_bind_instance_attribute",
        "_raised_class",
        "_exception_targets",
        "_raise_pending",
        "_active_exception",
        "_exception_instance",
        "_invalid_raise",
        "_install_builtin",
        "_install_storage",
        "_stdout_write",
        "_binding_missing",
        "_binding_count",
        "_binding_pos",
        "_binding_rest",
        "_binding_keyword_count",
        "_binding_keyword_name",
        "_binding_keyword_value",
        "_binding_default",
        "_allocate_instance",
        "_forward_call",
    )
}
_TEXT_ACCESS["_attribute_lookup"] = "mule_attribute_lookup_once"
_TEXT_ACCESS["_attribute_lookup_or"] = "mule_attribute_lookup_or"
_COMPILED_CALLS = {
    "_string_protocol": "mule_format_string",
    "_decimal_protocol": "mule_format_decimal_integer",
    "_percent_decimal_protocol": "mule_format_percent_decimal",
    "_format_value_protocol": "mule_format_format_value",
    "_representation_protocol": "mule_format_representation",
    "_hex_protocol": "mule_format__hex",
}
_EMITTERS = {
    "_instance_field": physical_field_read,
    "_set_instance_field": physical_field_write,
    "_dir_method": lookup_method("__dir__", include_object_defaults=True),
    **{name: lookup_method(method) for name, method in _LOOKUPS.items()},
    **{name: text_access(function) for name, function in _TEXT_ACCESS.items()},
    **{name: compiled_call(function) for name, function in _COMPILED_CALLS.items()},
    "_render_string": render_text(False),
    "_render_representation": render_text(True),
    "_text_result": text_result,
    "_numeric_modulo": numeric_modulo,
    "_allocate": allocate,
    "_initialize": initialize,
    "_invalid_initializer": invalid_initializer,
    "_read_int64": read_int64,
    "_write_int64": write_int64,
}


class _StorageLiteralLength(ast.Visitor):
    def visit_Node(self, node) -> int | None:
        return None

    def visit_Tuple(self, node: ast.Tuple) -> int:
        return len(node.elements)

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> int:
        return len(node.value)


_STORAGE_LITERAL_LENGTH = _StorageLiteralLength()


class _TextLiteralLength(ast.Visitor):
    def visit_Node(self, node) -> int | None:
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> int:
        return len(node.value)


_TEXT_LITERAL_LENGTH = _TextLiteralLength()


def _derive_storage_length(node) -> ValueType:
    if not _DERIVATION_MODE.active and len(node.args) == 1:
        seq_type = _arg_type(node.args[0])
        if seq_type is BOTTOM:
            return BOTTOM
        exact_len = seq_type.exact_length()
        if (
            exact_len is None
            and (lit := bound_constant_literal(node.args[0])) is not None
        ):
            exact_len = lit.accept(_STORAGE_LITERAL_LENGTH)
        if exact_len is not None:
            return IntegerType(exact_len, exact_len)
    return IntegerType(0, (1 << 60) - 1)


def _derive_text_length(node) -> ValueType:
    if not _DERIVATION_MODE.active and len(node.args) == 1:
        text_type = _arg_type(node.args[0])
        if text_type is BOTTOM:
            return BOTTOM
        exact_len = text_type.exact_length()
        if (
            exact_len is None
            and (lit := bound_constant_literal(node.args[0])) is not None
        ):
            exact_len = lit.accept(_TEXT_LITERAL_LENGTH)
        if exact_len is not None:
            return IntegerType(exact_len, exact_len)
    return IntegerType(0, (1 << 58) - 1)


_RESULT_TYPES = {
    "_instance_field": lambda node: None,
    "_tuple_storage": lambda node: BooleanType(),
    "_text_storage": lambda node: BooleanType(),
    "_storage_compatible": lambda node: BooleanType(),
    "_class_is_subclass": lambda node: BooleanType(),
    "_is_class": lambda node: BooleanType(),
    "_is_instance": lambda node: BooleanType(),
    "_storage_length": _derive_storage_length,
    "_text_length": _derive_text_length,
    "_text_codepoint": lambda node: IntegerType(0, 0x10FFFF),
    "_integer_storage": lambda node: IntegerType(),
    "_storage_hash": lambda node: OPTIONAL_FIXNUM,
    "_identity_hash": lambda node: IntegerType(FIXNUM_MIN, FIXNUM_MAX),
    "_text_character": lambda node: TEXT_TYPE,
    "_text_slice": lambda node: TEXT_TYPE,
    "_text_join": lambda node: TEXT_TYPE,
    "_text_from_codepoints": lambda node: TEXT_TYPE,
    "_type_name": lambda node: TEXT_TYPE,
    "_storage_tuple": lambda node: TupleType(),
    "_binding_missing": lambda node: UniqueObjectType("_binding_missing"),
    "_binding_count": lambda node: IntegerType(0, (1 << 60) - 1),
    "_binding_keyword_count": lambda node: IntegerType(0, (1 << 60) - 1),
    "_binding_keyword_name": lambda node: TEXT_TYPE,
}

_NON_INTEGER_FIRST_PRIMITIVES = frozenset(
    {
        "_storage_length",
        "_storage_read",
        "_storage_write",
        "_storage_append",
        "_storage_insert",
        "_storage_delete",
        "_storage_tuple",
    }
)

_BORROWED_PRIMITIVE_ARGS = {
    "_instance_field": frozenset({0, 1}),
    "_set_instance_field": frozenset({0, 1}),
    "_storage_reserve": frozenset({0, 1}),
    "_tuple_storage": frozenset({0}),
    "_text_storage": frozenset({0}),
    "_storage_compatible": frozenset({0, 1}),
    "_class_is_subclass": frozenset({0, 1}),
    "_is_class": frozenset({0}),
    "_is_instance": frozenset({0, 1}),
    "_class_lookup_default": frozenset({0, 1, 2}),
    "_bind_instance_attribute": frozenset({0, 1, 2}),
    "_storage_length": frozenset({0}),
    "_text_length": frozenset({0}),
    "_text_codepoint": frozenset({0, 1}),
    "_integer_storage": frozenset({0}),
    "_storage_hash": frozenset({0}),
    "_identity_hash": frozenset({0}),
    "_text_character": frozenset({0, 1}),
    "_text_slice": frozenset({0, 1, 2}),
    "_type_name": frozenset({0}),
    "_type_of": frozenset({0}),
    "_index_method": frozenset({0}),
    "_storage_read": frozenset({0, 1}),
    "_storage_write": frozenset({0, 1, 2}),
    "_storage_append": frozenset({0, 1}),
    "_storage_delete": frozenset({0, 1}),
}

PRIMITIVES = {
    name: defknown(
        name,
        *(("flushable",) if name in _RESULT_TYPES else ("call", "unsafe")),
        derive_type=_RESULT_TYPES.get(
            name,
            (lambda node: TEXT_TYPE) if name in _COMPILED_CALLS else None,
        ),
        borrowed_args=_BORROWED_PRIMITIVE_ARGS.get(name, frozenset()),
        non_integer_first_arg=(name in _NON_INTEGER_FIRST_PRIMITIVES),
    )
    for name in _EMITTERS
}
for _name, _emit in _EMITTERS.items():
    primitive(
        _name,
        PRIMITIVES[_name],
        _emit,
        consumes_arguments=_name in _COMPILED_CALLS or _name == "_initialize",
    )

# `_mule_native` exports whose generic entry only unpacks an argument tuple
# around one of these primitives. A protocol module whose only binding of a
# name is `from _mule_native import export` calls the primitive directly, as
# SBCL compiles a call to a known function through its VOP rather than its
# full-call entry. Keys cannot be identifiers, so they never match a name.
NATIVE_EXPORT_PRIMITIVES = {
    ("type_of", 1): "_type_of",
    ("is_subclass", 2): "_class_is_subclass",
    ("is_class", 1): "_is_class",
    ("is_instance", 2): "_is_instance",
    ("class_lookup_default", 3): "_class_lookup_default",
    ("bind_instance_attribute", 3): "_bind_instance_attribute",
}
for (_export, _arity), _name in NATIVE_EXPORT_PRIMITIVES.items():
    PRIMITIVES[f"_mule_native.{_export}/{_arity}"] = PRIMITIVES[_name]


@deftransform("_instance_field")
def _transform_instance_field(node):
    from .ir1opt import transform_physical_field_ref

    return transform_physical_field_ref(node)


@deftransform("type")
def _transform_type_of_value(node):
    """``type(x)`` is the class of ``x`` (SBCL's ``class-of``, a known
    function open-coded to the layout read). ``type.__new__`` answers a
    one-argument call through ``type`` itself with exactly this lookup, so
    the call through the metaclass protocol and its argument packing go
    away. The unmutated-builtin and keyword checks are the predictable-global
    test ``_OptimizeNode`` already made."""
    from .ir1util import retarget_known_combination

    if len(node.args) != 1:
        return False
    retarget_known_combination(node, PRIMITIVES["_type_of"], [node.args[0]])
    return True


@deftransform("_set_instance_field")
def _transform_set_instance_field(node):
    from .ir1opt import transform_physical_field_set

    return transform_physical_field_set(node)


@defpropagate("_instance_field")
def _propagate_instance_field(visitor, node):
    # Constraint analysis loads before protocol registration during startup.
    from .constraint import _propagate_instance_field as propagate

    return propagate(visitor, node)


@defpropagate("_set_instance_field")
def _propagate_set_instance_field(visitor, node):
    from .constraint import _propagate_set_instance_field as propagate

    return propagate(visitor, node)


@defpropagate("_storage_append")
def _propagate_storage_append(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 2:
        visitor.mutate_container_element(args[0], visitor.type_of(args[1]))
    return None


@defpropagate("_storage_write")
def _propagate_storage_write(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 3:
        seq_type = visitor.type_of(args[0])
        exact_len = seq_type.exact_length()
        max_idx = (
            exact_len - 1 if exact_len is not None and exact_len > 0 else (1 << 60) - 1
        )
        visitor.check_in_bounds_access(node, args[0], args[1])
        visitor.prove_argument_type(args[1], IntegerType(0, max_idx))
        visitor.mutate_container_element(
            args[0], visitor.type_of(args[2]), preserves_length=True
        )
    return None


@defpropagate("_storage_read")
def _propagate_storage_read(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 2:
        seq_type = visitor.type_of(args[0])
        idx_type = visitor.type_of(args[1])
        if seq_type is BOTTOM or idx_type is BOTTOM:
            visitor.walk.in_bounds_nodes.discard(node)
            return BOTTOM
        if seq_type == EMPTY_LIST_TYPE and idx_type.fits(PYTHON_INTEGER):
            visitor.walk.in_bounds_nodes.discard(node)
            return BOTTOM
        exact_len = seq_type.exact_length()
        max_idx = (
            exact_len - 1 if exact_len is not None and exact_len > 0 else (1 << 60) - 1
        )
        idx_bound = IntegerType(0, max_idx)
        visitor.check_in_bounds_access(node, args[0], args[1])
        visitor.prove_argument_type(args[1], idx_bound)
        return seq_type.getitem_type(idx_bound if idx_type is UNKNOWN else idx_type)
    return None


@defpropagate("_storage_length")
def _propagate_storage_length(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 1:
        seq_type = visitor.type_of(args[0])
        if seq_type is BOTTOM:
            return BOTTOM
        exact_len = seq_type.exact_length()
        if exact_len is None and (lit := bound_constant_literal(args[0])) is not None:
            exact_len = lit.accept(_STORAGE_LITERAL_LENGTH)
        if exact_len is not None:
            return IntegerType(exact_len, exact_len)
        return visitor.sequence_length_type(args[0])
    return None


@defpropagate("_text_length")
def _propagate_text_length(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 1:
        text_type = visitor.type_of(args[0])
        if text_type is BOTTOM:
            return BOTTOM
        visitor.prove_argument_type(args[0], TEXT_TYPE)
        exact_len = text_type.exact_length()
        if exact_len is None and (lit := bound_constant_literal(args[0])) is not None:
            exact_len = lit.accept(_TEXT_LITERAL_LENGTH)
        if exact_len is not None:
            return IntegerType(exact_len, exact_len)
        return visitor.sequence_length_type(args[0])
    return None


@defpropagate("_type_of")
def _propagate_type_of(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 1:
        src_var = visitor.origins.get(args[0])
        if src_var is not None and node.lvar is not None:
            visitor.type_of_origins[node.lvar] = src_var
        arg_type = visitor.type_of(args[0])
        if arg_type is BOTTOM:
            return BOTTOM
        return arg_type.runtime_class_type()
    return None


@defpropagate("_class_parent")
def _propagate_class_parent(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 1:
        arg_type = visitor.type_of(args[0])
        if arg_type is BOTTOM:
            return BOTTOM
        return arg_type.parent_class_type()
    return None


@defpropagate("_class_is_subclass")
def _propagate_class_is_subclass(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 2:
        left_type = visitor.type_of(args[0])
        right_type = visitor.type_of(args[1])
        if left_type is BOTTOM or right_type is BOTTOM:
            return BOTTOM
        res = left_type.is_subclass_of(right_type)
        if res is True:
            return BOOLEAN_TRUE
        if res is False:
            return BOOLEAN_FALSE
    return None


@defconstrain("_class_is_subclass")
def _constrain_class_is_subclass(visitor, node, state, truth: bool) -> bool:
    return _constrain_native_is_subclass(node, visitor, state, truth)


@defpropagate("_is_class")
def _propagate_is_class(visitor, node) -> ValueType | None:
    return _derive_native_is_class(tuple(visitor.type_of(arg) for arg in node.args))


@defpropagate("_is_instance")
def _propagate_is_instance(visitor, node) -> ValueType | None:
    return _derive_native_is_instance(tuple(visitor.type_of(arg) for arg in node.args))


@defconstrain("_is_instance")
def _constrain_is_instance(visitor, node, state, truth: bool) -> bool:
    return _constrain_native_is_instance(node, visitor, state, truth)


@defpropagate("_storage_compatible")
def _propagate_storage_compatible(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 2:
        left_type = visitor.type_of(args[0])
        right_type = visitor.type_of(args[1])
        if left_type is BOTTOM or right_type is BOTTOM:
            return BOTTOM
        if (
            left_type.fits_class()
            and right_type.fits_class()
            and left_type.compare_class_identity(right_type, True) is BOOLEAN_TRUE
        ):
            return BOOLEAN_TRUE
    return None


@defpropagate("_tuple_storage")
def _propagate_tuple_storage(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 1:
        arg_type = visitor.type_of(args[0])
        if arg_type is BOTTOM:
            return BOTTOM
        if not arg_type.may_be_unbound:
            if arg_type.fits_tuple(TupleType()):
                return BOOLEAN_TRUE
            if arg_type is not UNKNOWN:
                return BOOLEAN_FALSE
    return None


@defpropagate("_text_storage")
def _propagate_text_storage(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 1:
        arg_type = visitor.type_of(args[0])
        if arg_type is BOTTOM:
            return BOTTOM
        if not arg_type.may_be_unbound:
            if arg_type.fits_text(TEXT_TYPE):
                return BOOLEAN_TRUE
            if arg_type is not UNKNOWN:
                return BOOLEAN_FALSE
    return None


@defpropagate("_allocate_instance")
def _propagate_allocate_instance(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 1:
        arg_type = visitor.type_of(args[0])
        if arg_type is BOTTOM:
            return BOTTOM
        return visitor.walk.allocated_instance_type(arg_type)
    return None


@defpropagate("_text_codepoint")
def _propagate_text_codepoint(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 2:
        text_type = visitor.type_of(args[0])
        idx_type = visitor.type_of(args[1])
        if text_type is BOTTOM or idx_type is BOTTOM:
            return BOTTOM
        visitor.prove_argument_type(args[0], TEXT_TYPE)
        visitor.prove_argument_type(args[1], IntegerType(0, (1 << 58) - 1))
        idx = idx_type.exact_integer()
        if idx is not None:
            return text_type.codepoint_at(idx)
    return None


@defpropagate("_text_character")
def _propagate_text_character(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 2:
        text_type = visitor.type_of(args[0])
        idx_type = visitor.type_of(args[1])
        if text_type is BOTTOM or idx_type is BOTTOM:
            return BOTTOM
        visitor.prove_argument_type(args[0], TEXT_TYPE)
        visitor.prove_argument_type(args[1], IntegerType(0, (1 << 58) - 1))
        idx = idx_type.exact_integer()
        if idx is not None:
            cp_type = text_type.codepoint_at(idx)
            if cp_type is not None:
                cp = cp_type.exact_integer()
                if cp is not None:
                    return TextType(chr(cp))
        return TextType(length=1)
    return None


@defpropagate("_text_slice")
def _propagate_text_slice(visitor, node) -> ValueType | None:
    args = node.args
    if len(args) == 3:
        arg_types = [visitor.type_of(arg) for arg in args]
        if any(t is BOTTOM for t in arg_types):
            return BOTTOM
        visitor.prove_argument_type(args[0], TEXT_TYPE)
        visitor.prove_argument_type(args[1], IntegerType(0, 1 << 58))
        visitor.prove_argument_type(args[2], IntegerType(0, 1 << 58))
        start = arg_types[1].exact_integer()
        stop = arg_types[2].exact_integer()
        literal = arg_types[0].exact_text()
        if start is not None and stop is not None:
            if literal is not None:
                sliced = literal[start:stop]
                if len(sliced.encode("utf-8", "surrogatepass")) <= 64:
                    return TextType(sliced, interned=False)
            src_len = arg_types[0].exact_length()
            if src_len is not None and 0 <= start <= stop <= src_len:
                return TextType(length=stop - start, interned=False)
    return None


@defconstrain("_tuple_storage")
def _constrain_tuple_storage(visitor, node, state, truth: bool) -> bool:
    if len(node.args) == 1:
        src_var = visitor.origins.get(node.args[0])
        if src_var is not None and src_var in state:
            current = state[src_var]
            narrowed = current.type.narrow_by_runtime_class(CLASS_TUPLE_TYPE, truth)
            state[src_var] = current.with_type(narrowed)
            return True
    return False


@defconstrain("_text_storage")
def _constrain_text_storage(visitor, node, state, truth: bool) -> bool:
    if len(node.args) == 1:
        src_var = visitor.origins.get(node.args[0])
        if src_var is not None and src_var in state:
            current = state[src_var]
            narrowed = current.type.narrow_by_runtime_class(CLASS_STR_TYPE, truth)
            state[src_var] = current.with_type(narrowed)
            return True
    return False


NATIVE_MODULE_EXPORTS: dict[tuple[str, str], ValueType] = {
    ("_mule_native", "text_from_codepoints"): TEXT_TYPE,
    ("_mule_native", "bytes_from_values"): BYTES_TYPE,
    ("_mule_native", "byte_copy"): BYTES_TYPE,
    ("_mule_native", "byte_length"): IntegerType(0, (1 << 48) - 1),
    ("_mule_native", "read_byte"): IntegerType(0, 255),
    ("_mule_native", "identity"): INTEGER,
    ("_mule_native", "identity_hash"): IntegerType(FIXNUM_MIN, FIXNUM_MAX),
    ("_mule_native", "is_class"): BOOLEAN,
    ("_mule_native", "is_subclass"): BOOLEAN,
    ("_mule_native", "is_instance"): BOOLEAN,
    ("_mule_native", "overriding_new"): BOOLEAN,
    ("_mule_native", "buffer_exports"): IntegerType(0, (1 << 48) - 1),
    ("_mule_native", "numeric_invert"): INTEGER,
    ("_mule_native", "integer_decimal"): TEXT_TYPE,
    ("_mule_native", "math_fabs"): FLOAT,
    ("_mule_native", "math_to_double"): FLOAT,
    ("_mule_native", "math_sqrt"): FLOAT,
    ("_mule_native", "math_log"): FLOAT,
    ("_mule_native", "math_log2"): FLOAT,
    ("_mule_native", "math_exp"): FLOAT,
    ("_mule_native", "math_sin"): FLOAT,
    ("_mule_native", "math_cos"): FLOAT,
    ("_mule_native", "math_acos"): FLOAT,
    ("_mule_native", "math_lgamma"): FLOAT,
    ("_mule_native", "math_number_kind"): IntegerType(0, 2),
    ("_mule_native", "math_integer_value"): INTEGER,
    ("_mule_native", "math_integer_bit_length"): IntegerType(0, (1 << 48) - 1),
    ("_mule_native", "math_float_bits"): INTEGER,
    ("_mule_native", "math_float_format"): TEXT_TYPE,
    ("_mule_native", "intern_text"): INTERNED_TEXT_TYPE,
    ("_mule_native", "is_interned_text"): BOOLEAN,
}


def _propagate_native_type_of(node, visitor, args, arg_types, keywords=()) -> ValueType:
    if len(args) == 1:
        src_var = visitor.origins.get(args[0])
        if src_var is not None and node.lvar is not None:
            visitor.type_of_origins[node.lvar] = src_var
        if arg_types[0] is BOTTOM:
            return BOTTOM
        return arg_types[0].runtime_class_type()
    return UNKNOWN


def _derive_native_type_of(arg_types, keywords=(), *, receiver_type=None) -> ValueType:
    if len(arg_types) == 1:
        if arg_types[0] is BOTTOM:
            return BOTTOM
        return arg_types[0].runtime_class_type()
    return UNKNOWN


def _derive_native_is_class(arg_types, keywords=(), *, receiver_type=None) -> ValueType:
    if len(arg_types) == 1:
        if arg_types[0] is BOTTOM:
            return BOTTOM
        res = arg_types[0].is_class_object()
        if res is True:
            return BOOLEAN_TRUE
        if res is False:
            return BOOLEAN_FALSE
    return BOOLEAN


def _derive_native_is_subclass(
    arg_types, keywords=(), *, receiver_type=None
) -> ValueType:
    if len(arg_types) == 2:
        if arg_types[0] is BOTTOM or arg_types[1] is BOTTOM:
            return BOTTOM
        res = arg_types[0].is_subclass_of(arg_types[1])
        if res is True:
            return BOOLEAN_TRUE
        if res is False:
            return BOOLEAN_FALSE
    return BOOLEAN


def _constrain_native_is_subclass(node, visitor, state, truth: bool) -> bool:
    if len(node.args) == 2:
        cls_var = visitor.origins.get(node.args[0])
        if cls_var is not None and cls_var in visitor.state:
            target_type = visitor.lvar_fact(node.args[1]).type
            current = visitor.state[cls_var]
            narrowed = current.type.narrow_identity(target_type, truth)
            if narrowed is not current.type:
                state[cls_var] = visitor.narrowed_fact(current, narrowed)
            src_var = visitor.type_of_origins.get(node.args[0])
            if src_var is not None and src_var in visitor.state:
                src_curr = visitor.state[src_var]
                src_narrow = src_curr.type.narrow_by_runtime_class(target_type, truth)
                if src_narrow is not src_curr.type:
                    state[src_var] = visitor.narrowed_fact(src_curr, src_narrow)
            return True
    return False


def _derive_native_is_instance(
    arg_types, keywords=(), *, receiver_type=None
) -> ValueType:
    if len(arg_types) == 2:
        if arg_types[0] is BOTTOM or arg_types[1] is BOTTOM:
            return BOTTOM
        res = arg_types[0].is_instance_of(arg_types[1])
        if res is True:
            return BOOLEAN_TRUE
        if res is False:
            return BOOLEAN_FALSE
    return BOOLEAN


def _constrain_native_is_instance(node, visitor, state, truth: bool) -> bool:
    if len(node.args) == 2:
        src_var = visitor.origins.get(node.args[0])
        if src_var is not None and src_var in visitor.state:
            target_type = visitor.lvar_fact(node.args[1]).type
            src_curr = visitor.state[src_var]
            src_narrow = src_curr.type.narrow_by_runtime_class(target_type, truth)
            if src_narrow is not src_curr.type:
                state[src_var] = visitor.narrowed_fact(src_curr, src_narrow)
            return True
    return False


def _derive_native_byte_length(
    arg_types, keywords=(), *, receiver_type=None
) -> ValueType:
    if len(arg_types) == 1:
        if arg_types[0] is BOTTOM:
            return BOTTOM
        exact_len = arg_types[0].exact_length()
        if exact_len is not None:
            return IntegerType(exact_len, exact_len)
    return IntegerType(0, (1 << 48) - 1)


def _propagate_native_read_byte(
    node, visitor, args, arg_types, keywords=()
) -> ValueType:
    if len(args) == 2:
        if arg_types[0] is BOTTOM or arg_types[1] is BOTTOM:
            return BOTTOM
        visitor.prove_argument_type(args[1], IntegerType(0, (1 << 48) - 1))
    return IntegerType(0, 255)


def _derive_native_typed_value(
    arg_types, keywords=(), *, receiver_type=None
) -> ValueType:
    if len(arg_types) >= 1:
        return arg_types[0]
    return UNKNOWN


def _derive_native_intern_text(
    arg_types, keywords=(), *, receiver_type=None
) -> ValueType:
    if len(arg_types) == 1:
        if arg_types[0] is BOTTOM:
            return BOTTOM
        exact = arg_types[0].exact_text()
        if exact is not None:
            return TextType(exact, interned=True)
    return INTERNED_TEXT_TYPE


def _derive_native_is_interned_text(
    arg_types, keywords=(), *, receiver_type=None
) -> ValueType:
    if len(arg_types) == 1:
        if arg_types[0] is BOTTOM:
            return BOTTOM
        if arg_types[0].is_interned_text:
            return BOOLEAN_TRUE
    return BOOLEAN


NATIVE_MODULE_TARGETS: dict[tuple[str, str], ForeignCallTarget] = {
    key: ForeignCallTarget(return_type=ret_type)
    for key, ret_type in NATIVE_MODULE_EXPORTS.items()
}
NATIVE_MODULE_TARGETS[("_mule_native", "intern_text")] = ForeignCallTarget(
    return_type=INTERNED_TEXT_TYPE,
    derive_fn=_derive_native_intern_text,
)
NATIVE_MODULE_TARGETS[("_mule_native", "is_interned_text")] = ForeignCallTarget(
    return_type=BOOLEAN,
    derive_fn=_derive_native_is_interned_text,
)
NATIVE_MODULE_TARGETS[("_mule_native", "type_of")] = ForeignCallTarget(
    derive_fn=_derive_native_type_of,
    propagate_fn=_propagate_native_type_of,
)
NATIVE_MODULE_TARGETS[("_mule_native", "is_class")] = ForeignCallTarget(
    return_type=BOOLEAN,
    derive_fn=_derive_native_is_class,
)
NATIVE_MODULE_TARGETS[("_mule_native", "is_subclass")] = ForeignCallTarget(
    return_type=BOOLEAN,
    derive_fn=_derive_native_is_subclass,
    constrain_fn=_constrain_native_is_subclass,
)
NATIVE_MODULE_TARGETS[("_mule_native", "is_instance")] = ForeignCallTarget(
    return_type=BOOLEAN,
    derive_fn=_derive_native_is_instance,
    constrain_fn=_constrain_native_is_instance,
)
NATIVE_MODULE_TARGETS[("_mule_native", "byte_length")] = ForeignCallTarget(
    return_type=IntegerType(0, (1 << 48) - 1),
    derive_fn=_derive_native_byte_length,
)
NATIVE_MODULE_TARGETS[("_mule_native", "read_byte")] = ForeignCallTarget(
    return_type=IntegerType(0, 255),
    propagate_fn=_propagate_native_read_byte,
)
NATIVE_MODULE_TARGETS[("_mule_native", "typed_value")] = ForeignCallTarget(
    return_type=UNKNOWN,
    derive_fn=_derive_native_typed_value,
)


def compile_protocol(
    source: str,
    filename: str,
    *,
    prefix: str = "",
    namespace: str,
    imports=None,
    primitive_data=None,
):
    """Compile a protocol module through the IR1 phases with the primitives
    bound. Returns the ``Compilation``."""
    compilation = compile_toplevel(
        source,
        filename,
        imports=imports,
        prefix=prefix,
        namespace=namespace,
        primitives=PRIMITIVES,
        primitive_data=primitive_data,
    )
    if imports is not None:
        compilation.imported_function_types = imports.exported_types
        compilation.imported_targets = imports.exported_targets
        compilation.storage_methods = imports.storage_methods
        compilation.builtin_return_types = imports.builtin_return_types
        compilation.builtin_targets = imports.builtin_targets
    else:
        compilation.imported_function_types = NATIVE_MODULE_EXPORTS
        compilation.imported_targets = NATIVE_MODULE_TARGETS
    ir1_phases(compilation)
    return compilation


def add_lambdas(program, lambdas, facts) -> None:
    program.lambdas.extend(lambdas)
    for lambda_ in lambdas:
        program.facts[lambda_] = facts[lambda_]


__all__ = [
    "NATIVE_MODULE_EXPORTS",
    "NATIVE_MODULE_TARGETS",
    "PRIMITIVES",
    "add_lambdas",
    "compile_protocol",
]
