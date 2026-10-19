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

from .ctype import (
    FIXNUM_MAX,
    FIXNUM_MIN,
    OPTIONAL_FIXNUM,
    TEXT_TYPE,
    BooleanType,
    IntegerType,
    TupleType,
)
from .fndb import defknown
from .main import compile_toplevel, ir1_phases
from .vops import (
    allocate,
    compiled_call,
    initialize,
    invalid_initializer,
    lookup_method,
    numeric_modulo,
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
_COMPILED_CALLS = {
    "_string_protocol": "mule_format_string",
    "_decimal_protocol": "mule_format_decimal_integer",
    "_percent_decimal_protocol": "mule_format_percent_decimal",
    "_format_value_protocol": "mule_format_format_value",
    "_representation_protocol": "mule_format_representation",
    "_hex_protocol": "mule_format__hex",
}
_EMITTERS = {
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

_RESULT_TYPES = {
    "_tuple_storage": lambda node: BooleanType(),
    "_text_storage": lambda node: BooleanType(),
    "_storage_compatible": lambda node: BooleanType(),
    "_class_is_subclass": lambda node: BooleanType(),
    "_storage_length": lambda node: IntegerType(0, (1 << 60) - 1),
    "_text_length": lambda node: IntegerType(0, (1 << 58) - 1),
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
    "_binding_count": lambda node: IntegerType(0, (1 << 60) - 1),
    "_binding_keyword_count": lambda node: IntegerType(0, (1 << 60) - 1),
    "_binding_keyword_name": lambda node: TEXT_TYPE,
}

PRIMITIVES = {
    name: defknown(
        name,
        *(() if name in _RESULT_TYPES else ("call", "unsafe")),
        derive_type=_RESULT_TYPES.get(
            name,
            (lambda node: TEXT_TYPE) if name in _COMPILED_CALLS else None,
        ),
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
    ir1_phases(compilation)
    return compilation


def add_lambdas(program, lambdas, facts) -> None:
    program.lambdas.extend(lambdas)
    for lambda_ in lambdas:
        program.facts[lambda_] = facts[lambda_]


__all__ = ["PRIMITIVES", "add_lambdas", "compile_protocol"]
