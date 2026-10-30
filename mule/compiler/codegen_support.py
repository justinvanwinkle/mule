"""LLVM runtime declarations, foreign function metadata, slot lifetime packing, and helper builders."""

from __future__ import annotations

import logging
import re
import struct
from contextlib import contextmanager
from dataclasses import dataclass, field
from functools import cache
from typing import TYPE_CHECKING

from mule import ast, procedures
from mule.diagnostics import phase

from . import vops, wordops
from .ctype import (
    BOTTOM,
    FIXNUM_MAX,
    FIXNUM_MIN,
    UNKNOWN,
    ListType,
    TextType,
    ValueType,
    ValueTypeVisitor,
)
from .escape import analyze_escapes
from .ir1util import made_function
from .ir2tran import _specialize_list_getitems, ir2_convert
from .locall import specialized_ir1_context
from .ltn import ltn_analyze
from .node import (
    CallKind,
    CodeInfo,
    Combination,
    Lambda,
    NodeVisitor,
    Signature,
    linked,
)
from .represent import (
    assign_variable_primitive_types,
    emit_moves_and_coercions,
    select_representations,
)
from .scalar import replace_local_aggregates
from .vop import (
    DESCRIPTOR,
)

if TYPE_CHECKING:
    from .ir1tran import ImportInfo
    from .native_protocol import Layout


OBJECT = vops.OBJECT
logger = logging.getLogger(__name__)


def _llvm_comment(text: str) -> str:
    """Keep source filenames and other provenance on one LLVM comment line."""
    return "; " + text.encode("unicode_escape").decode("ascii")


def _function_comment(code: CodeInfo, entry: str) -> str:
    return _llvm_comment(
        f"source: {code.filename}:{code.firstlineno}; "
        f"function: {code.qualname}; entry: {entry}"
    )


def _helper_comment(purpose: str) -> str:
    return _llvm_comment(f"helper: {purpose}; emitter: mule/compiler/codegen.py")


def _without_llvm_comments(text: str) -> str:
    """Provenance must not create symbol dependencies from source filenames."""
    return "\n".join(
        line for line in text.splitlines() if not line.lstrip().startswith(";")
    )


class _ProvenanceOperation(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node):
        if node.fun_info is not None:
            return node.fun_info.name
        return f"{node.kind.name.lower()} call"


_PROVENANCE_OPERATION = _ProvenanceOperation()


DECLARATIONS = """declare ptr @mule_literal_integer(ptr)
declare ptr @mule_literal_float(double)
declare ptr @mule_literal_text(ptr, i64)
declare ptr @mule_literal_bytes(ptr, i64)
declare ptr @mule_none()
declare ptr @mule_bool(i32)
declare ptr @mule_load_global(ptr, ptr)
declare i32 @mule_store_global(ptr, ptr, ptr, i32)
declare i32 @mule_link_globals(ptr, i64, ptr, ptr, i32)
declare void @mule_traceback_here(ptr, i32, ptr, ptr, i32, ptr) cold
declare ptr @mule_frame_head_slot()
declare ptr @mule_closure_slots(ptr)
@mule_safepoint_requested = external global i8
declare i32 @mule_safepoint() cold
declare ptr @mule_namespace()
declare i32 @mule_namespace_bind(ptr, ptr, ptr)
declare i32 @mule_namespace_bound(ptr, ptr)
declare i32 @mule_assertion_failure(ptr) cold
declare ptr @mule_sequence_list(i64, ptr)
declare ptr @mule_sequence_tuple(i64, ptr)
declare ptr @mule_sequence_tuple_owned(i64, ptr)
declare ptr @mule_lookup_special(ptr, ptr)
declare ptr @mule_lookup_special_including_defaults(ptr, ptr)
declare ptr @mule_render_text(ptr, i32)
declare ptr @mule_text_result(ptr)
declare ptr @mule_text_length(ptr)
declare ptr @mule_text_character(ptr, ptr)
declare ptr @mule_text_char_at(ptr, i64)
declare ptr @mule_text_codepoint(ptr, ptr)
declare ptr @mule_text_codepoint_at(ptr, i64)
declare ptr @mule_text_codepoint_getitem(ptr, ptr)
declare ptr @mule_text_slice(ptr, ptr, ptr)
declare ptr @mule_text_find(ptr, ptr, ptr, ptr, i32)
declare i32 @mule_text_eq(ptr, ptr)
declare i32 @mule_text_partition_into(ptr, ptr, i32, ptr)
declare ptr @mule_text_partition(ptr, ptr, i32)
declare ptr @mule_text_split(ptr, ptr, ptr)
declare ptr @mule_text_affix(ptr, ptr, ptr, ptr, i32)
declare ptr @mule_text_join(ptr)
declare ptr @mule_text_from_codepoints(ptr)
declare ptr @mule_text_decode_bytes(ptr, ptr)
declare ptr @mule_text_encode_bytes(ptr, ptr)
declare ptr @mule_storage_hash(ptr)
declare ptr @mule_identity_hash(ptr)
declare ptr @mule_type_of(ptr)
declare ptr @mule_type_name(ptr)
declare ptr @mule_load_local(ptr, ptr)
declare ptr @mule_binary_reuse(i32, ptr, ptr, i32, ptr)
declare ptr @mule_unary(i32, ptr)
declare ptr @mule_compare(i32, ptr, ptr)
declare ptr @mule_compare_bool(i32, ptr, ptr)
declare i32 @mule_truth(ptr)
declare ptr @mule_call(ptr, i64, ptr)
declare ptr @mule_call_owned(ptr, i64, ptr)
declare ptr @mule_call_keywords(ptr, ptr, i64, ptr, i64, ptr, ptr)
declare ptr @mule_function_defaults(ptr, ptr, ptr, ptr)
declare ptr @mule_function_default(ptr, i32)
declare void @mule_set_argument_constructor(ptr)
declare ptr @mule_construct_arguments(ptr, i64, ptr)
declare i32 @mule_binding_positional(ptr, i32, i32, i32)
declare ptr @mule_arg_or_default(ptr, i32, i32)
declare ptr @mule_binding_default_at(ptr, i32)
declare i64 @mule_binding_view(ptr, ptr, i64, ptr, ptr, ptr)
declare i64 @mule_binding_name_index(ptr, ptr, i64)
declare void @mule_binding_error(i32, ptr)
declare ptr @mule_binding_keyword_text(ptr, i64)
declare ptr @mule_binding_missing()
declare ptr @mule_binding_count(ptr)
declare ptr @mule_binding_pos(ptr, ptr)
declare ptr @mule_binding_rest(ptr, ptr)
declare ptr @mule_binding_keyword_count(ptr)
declare ptr @mule_binding_keyword_name(ptr, ptr)
declare ptr @mule_binding_keyword_value(ptr, ptr)
declare ptr @mule_binding_default(ptr, ptr)
declare ptr @mule_allocate_instance(ptr)
declare ptr @mule_forward_call(ptr, ptr)
declare ptr @mule_function(ptr, ptr, ptr)
declare void @mule_set_function_metadata(ptr, ptr, ptr)
declare ptr @mule_function_closure(ptr, ptr, ptr)
declare ptr @mule_closure(ptr)
declare ptr @mule_function_closure_of(ptr)
declare ptr @mule_class(ptr, ptr, i64, ptr, ptr, ptr)
declare i32 @mule_class_store(ptr, ptr, ptr)
declare ptr @mule_class_prepare(ptr, ptr, ptr)
declare ptr @mule_native_class(ptr, ptr, ptr, i64, ptr, ptr, ptr, ptr, ptr)
declare ptr @mule_class_load(ptr, ptr, ptr)
declare ptr @mule_class_load_or(ptr, ptr, ptr)
declare i32 @mule_import_star(ptr, ptr)
declare ptr @mule_coroutine_yield(ptr)
declare void @mule_coroutine_set_locals(ptr, i64)
declare i32 @mule_class_delete(ptr, ptr, i32)
declare i32 @mule_native_class_matches(ptr, ptr, ptr)
declare i32 @mule_native_instance_matches(ptr, ptr, ptr)
declare ptr @mule_native_instance_payload(ptr, ptr, ptr)
declare ptr @mule_native_new(ptr)
declare ptr @mule_native_payload(ptr)
declare i32 @mule_unbox_i64(ptr, ptr)
declare ptr @mule_box_i64(i64)
declare i32 @mule_unbox_f64(ptr, ptr)
declare ptr @mule_box_f64(double)
declare noalias nonnull align 8 ptr @mule_allocate_object_storage() allockind("alloc,uninitialized") "alloc-family"="mule_allocate_object_storage"
declare void @mule_deallocate_object_storage(ptr allocptr) allockind("free") "alloc-family"="mule_allocate_object_storage"
@__gmp_bits_per_limb = external constant i32
declare noalias nonnull align 8 ptr @mule_allocate_integer_limbs(i64) allocsize(0) allockind("alloc,uninitialized") "alloc-family"="mule_allocate_integer_limbs"
declare void @mule_deallocate_integer_limbs(ptr allocptr, i64) allockind("free") "alloc-family"="mule_allocate_integer_limbs"
declare void @mule_clear_integer_storage(ptr)
declare ptr @mule_box_f64_move(double, ptr, ptr)
declare i32 @mule_native_zero_division() cold
declare i32 @mule_native_type_error() cold
declare i32 @mule_native_initializer_error() cold
declare i32 @mule_native_overflow() cold
declare ptr @mule_get_attr_cached(ptr, ptr, ptr)
declare ptr @mule_instance_field(ptr, ptr)
declare ptr @mule_set_instance_field(ptr, ptr, ptr)
declare void @mule_instance_field_missing(ptr, ptr, i64) cold
declare ptr @mule_instance_field_name(ptr, ptr, i64)
declare ptr @mule_instance_field_name_cached(ptr, ptr, i64, ptr)
declare ptr @mule_set_instance_field_name(ptr, ptr, i64, ptr)
declare ptr @mule_set_instance_field_name_cached(ptr, ptr, i64, ptr, ptr)
declare i64 @mule_instance_layout_id(ptr) nounwind readonly
declare i64 @mule_reference_layout_id(ptr) nounwind readonly
declare i32 @mule_set_attr_cached(ptr, ptr, ptr, ptr)
declare ptr @mule_resolve_method(ptr, ptr, ptr, ptr)
declare ptr @mule_resolve_super_method(ptr, ptr, ptr, ptr, ptr, ptr)
declare ptr @mule_call_resolved(ptr, ptr, i64, ptr)
declare ptr @mule_call_resolved_owned(ptr, ptr, i64, ptr)
declare i32 @mule_check_args(ptr, i32)
declare i32 @mule_assert_integer(ptr, ptr, ptr, ptr)
declare i32 @mule_assert_float(ptr, ptr)
declare i32 @mule_assert_real(ptr, ptr)
declare i32 @mule_value_kind(ptr) nounwind willreturn memory(read)
declare i32 @mule_integer_in_bounds(ptr, ptr, ptr)
declare ptr @mule_tuple_borrow_item(ptr, i64) nounwind willreturn memory(read)
declare i32 @mule_inferred_return_error(ptr) cold
declare ptr @mule_arg(ptr, i32)
declare ptr @mule_retain(ptr returned)
declare void @mule_release(ptr)
declare void @mule_destroy(ptr)
declare i32 @mule_enter_frame()
declare void @mule_leave_frame()
declare i32 @mule_function_matches(ptr, ptr, ptr)
declare ptr @mule_method_entry(ptr, ptr)
declare i32 @mule_run_protocols(ptr, ptr)
declare i32 @mule_range_storage(ptr, ptr)
declare ptr @mule_protocol_globals()
declare ptr @mule_catch_exception()
declare i32 @mule_enter_handler(ptr)
declare void @mule_leave_handler()
declare i32 @mule_delete_global(ptr, ptr)
declare i32 @mule_unbind_global(ptr, ptr)
declare i32 @mule_reraise() cold
declare i32 @mule_check_rest_args(ptr, i32)
declare ptr @mule_rest_args(ptr, i32)
declare ptr @mule_fast_len(ptr)
declare ptr @mule_parse_int_text(ptr)
declare ptr @mule_parse_int_slice(ptr, ptr, ptr)
declare ptr @mule_storage_length(ptr)
declare void @mule_storage_track(ptr, ptr)
declare ptr @mule_storage_read(ptr, ptr)
declare ptr @mule_storage_write(ptr, ptr, ptr)
declare ptr @mule_fast_getitem(ptr, ptr)
declare ptr @mule_list_getitem(ptr, ptr)
declare ptr @mule_list_getitem_borrow(ptr, ptr)
declare ptr @mule_bytes_getitem(ptr, ptr)
declare ptr @mule_bytearray_getitem_fast(ptr, ptr)
declare i32 @mule_bytearray_setitem_fast(ptr, ptr, ptr)
declare ptr @mule_bytearray_append_fast(ptr, ptr)
declare ptr @mule_byte_storage_new()
declare i32 @mule_byte_storage_reserve(ptr, i64)
declare i32 @mule_bytes_storage(ptr, ptr)
declare i32 @mule_text_cursor_storage(ptr, ptr)
declare ptr @mule_byte_length(ptr)
declare ptr @mule_read_byte(ptr, ptr)
declare ptr @mule_read_byte_at(ptr, i64)
declare i32 @mule_math_domain_error()
declare i32 @mule_unbox_real(ptr, ptr)
declare ptr @mule_fast_getslice(ptr, ptr, ptr, ptr)
declare ptr @mule_text_slice_eq(ptr, ptr, ptr, ptr, i32)
declare ptr @mule_text_slice2_eq(ptr, ptr, ptr, ptr, ptr, ptr, i32)
declare i32 @mule_fast_setitem(ptr, ptr, ptr)
declare i32 @mule_fast_setitem_move(ptr, ptr, ptr)
declare i32 @mule_fast_next_item(ptr, ptr)
declare i32 @mule_fast_next_unpack(ptr, i64, ptr, ptr)
declare ptr @mule_fast_iterate(ptr)
declare ptr @mule_fast_iterate_move(ptr)
declare i32 @mule_fast_unpack(ptr, i64, ptr)
declare ptr @mule_storage_append(ptr, ptr)
declare ptr @mule_storage_reserve(ptr, ptr)
declare ptr @mule_storage_append_move(ptr, ptr)
declare ptr @mule_storage_insert(ptr, ptr, ptr)
declare ptr @mule_storage_delete(ptr, ptr, ptr)
declare ptr @mule_storage_pop(ptr)
declare ptr @mule_storage_tuple(ptr)
declare ptr @mule_tuple_storage(ptr)
declare i32 @mule_tuple_shape_matches(ptr, i64) nounwind willreturn memory(read)
declare i32 @mule_list_shape_matches(ptr, i64) nounwind willreturn memory(read)
declare i32 @mule_exact_list(ptr) nounwind willreturn memory(read)
declare i32 @mule_exact_text(ptr) nounwind willreturn memory(read)
declare nonnull ptr @mule_tuple_slot(ptr, i64)
declare ptr @mule_storage_compatible(ptr, ptr)
declare ptr @mule_integer_storage(ptr)
declare ptr @mule_class_identity(ptr)
declare ptr @mule_class_parent(ptr)
declare ptr @mule_class_is_subclass(ptr, ptr)
declare ptr @mule_is_class(ptr)
declare ptr @mule_is_instance(ptr, ptr)
declare ptr @mule_class_lookup_default(ptr, ptr, ptr)
declare ptr @mule_bind_instance_attribute(ptr, ptr, ptr)
declare ptr @mule_raised_class(ptr)
declare ptr @mule_exception_targets(ptr)
declare ptr @mule_raise_pending(ptr) cold
declare ptr @mule_active_exception()
declare ptr @mule_exception_instance(ptr)
declare ptr @mule_invalid_raise() cold
declare ptr @mule_install_builtin(ptr, ptr)
declare ptr @mule_text_storage(ptr)
declare ptr @mule_stdout_write(ptr)
declare ptr @mule_install_storage(ptr, ptr, ptr, ptr, ptr)
declare void @mule_set_process_arguments(i32, ptr)
declare ptr @mule_native_module_init(ptr)
declare ptr @mule_posix_module_init(ptr)
declare ptr @mule_time_module_init(ptr)
declare ptr @mule_builtins_module()
declare ptr @mule_import_missing(ptr)
declare ptr @mule_import_loaded(ptr)
declare void @mule_module_loaded(ptr, ptr)
declare void @mule_module_failed(ptr)
declare void @mule_register_compiled_module(ptr, ptr, ptr, ptr, ptr, ptr, i64)
declare void @mule_set_image_search_path(ptr, i64)
declare void @mule_set_image_toolchain(ptr, ptr, ptr, i64)
declare void @mule_set_attribute_lookup(ptr)
declare ptr @mule_attribute_lookup_once(ptr, ptr)
declare ptr @mule_attribute_lookup_or(ptr, ptr, ptr)
declare ptr @mule_getattr_default(ptr, ptr, ptr, ptr)
declare ptr @mule_hasattr(ptr, ptr, ptr)
declare ptr @mule_isinstance_single(ptr, ptr)
declare ptr @mule_isinstance_any(ptr, ptr, i64)
declare ptr @mule_builtins_module_init(ptr)
declare void @mule_register_finalizer(ptr)
declare void @mule_run_finalizers()
declare i32 @mule_run(ptr)
declare void @llvm.assume(i1)
"""

_HEX_BYTE = tuple(f"\\{byte:02X}" for byte in range(256))
_MULE_SYMBOL_RE = re.compile(r"@mule[._][A-Za-z0-9_.]*\(?")


def _declaration_symbol(line: str) -> tuple[str, str] | None:
    stripped = line.strip()
    if not stripped or stripped.startswith(";"):
        return None
    symbol = (
        stripped.split()[0]
        if stripped.startswith("@")
        else stripped.split("(")[0].split()[-1]
    )
    return symbol, line


_DECLARATION_LINES = tuple(
    entry
    for line in DECLARATIONS.splitlines()
    if (entry := _declaration_symbol(line)) is not None
)


@cache
def _parsed_helpers(
    bootstrap: bool,
) -> tuple[tuple[tuple[str, str], ...], tuple[tuple[str, str], ...]]:
    intrinsics_lines: list[tuple[str, str]] = []
    helper_defs: list[tuple[str, str]] = []
    current_symbol = None
    current_lines: list[str] = []
    for emitter, helper_text in (
        *procedures.all_procedures(),
        ("mule/compiler/wordops.py", wordops.helpers()),
    ):
        for line in helper_text.splitlines():
            if current_symbol is not None:
                current_lines.append(line)
                if line == "}":
                    helper_defs.append((current_symbol, "\n".join(current_lines)))
                    current_symbol = None
                    current_lines = []
            elif line.startswith("define "):
                current_symbol = line.split("(")[0].split()[-1]
                current_lines = [
                    _llvm_comment(f"helper: {current_symbol}; emitter: {emitter}"),
                    line,
                ]
            elif (entry := _declaration_symbol(line)) is not None:
                intrinsics_lines.append(entry)
    return tuple(intrinsics_lines), tuple(helper_defs)


@dataclass(frozen=True, slots=True, eq=False)
class ForeignFunction:
    """A function compiled in another unit, as the units calling it see it:
    its symbols derive from ``name``; ``signature`` maps static calls."""

    name: str
    parameters: tuple[str, ...]
    signature: Signature
    takes_closure: bool
    return_type: ValueType = UNKNOWN
    namespace: str = ""
    borrowed_parameters: frozenset[int] = frozenset()
    entry_parameter_types: tuple[ValueType, ...] | None = None
    default_types: tuple[ValueType, ...] = ()

    def as_lambda(self) -> None:
        return None

    def as_function(self) -> ForeignFunction:
        return self

    def implementation_type(self, domain: ValueType) -> ValueType:
        return UNKNOWN

    def supports_escape_summary(self) -> bool:
        return not self.signature.variadic

    def classify_call_escape(
        self, solver, source, param_index: int, *, direct_positional: bool = True
    ) -> None:
        if param_index in self.borrowed_parameters:
            solver.allocation_only_escape(source)
        else:
            solver.escape(source)

    def global_constant_type(self) -> ValueType:
        return UNKNOWN

    def summary_return_type(self) -> ValueType:
        return self.return_type

    def derive_return_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType:
        if any(arg is BOTTOM for arg in arg_types) or receiver_type is BOTTOM:
            return BOTTOM
        return self.return_type

    def derive_self_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType | None:
        return None

    def constrain_call(self, node, visitor, state, truth: bool) -> bool:
        return False

    def propagate_call(
        self,
        node,
        visitor,
        args,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
    ) -> ValueType:
        return self.derive_return_type(arg_types, keywords)

    def to_interface_entry(self) -> dict:
        return {
            "kind": "function",
            "name": self.name,
            "entry_parameter_types": (
                None
                if self.entry_parameter_types is None
                else [fact.to_dict() for fact in self.entry_parameter_types]
            ),
        }

    def call_order(self, keywords, count, *, receiver=False):
        return self.signature.call_order(
            self.parameters, keywords, count, receiver=receiver
        )


# A function a unit defines or calls in another unit.
type Function = Lambda | ForeignFunction


@dataclass(slots=True)
class Program:
    """Everything code generation needs for one unit: the lambdas it defines
    in emission order, their facts, and the compiled-protocol entry points.

    ``external`` are other units' lambdas this unit may call. A unit with an
    ``entry`` is the executable's main unit; ``exports`` makes its bodies
    callable from other units."""

    lambdas: list[Lambda]
    entry: Lambda | None
    facts: dict
    print_entry: Function | None = None
    format_entry: Function | None = None
    protocol_entry: Function | None = None
    sequence_functions: dict[str, Function] = field(default_factory=dict)
    argument_constructor: Function | None = None
    argument_binder: Function | None = None
    layouts: dict[str, Layout] = field(default_factory=dict)
    # Names the builtins unit installs as builtins, which later units can
    # copy once they link.
    builtin_names: frozenset[str] = frozenset()
    # The builtins unit's code runs while its protocols install the builtins,
    # replacing runtime-provided ones such as ``tuple``, so it reads globals
    # live whatever its safety.
    bootstrap: bool = False
    external: list[ForeignFunction] = field(default_factory=list)
    external_facts: dict = field(default_factory=dict)
    unit: str = "program"
    exports: bool = False
    # Units the main unit initializes before its own, in order.
    dependencies: list[str] = field(default_factory=list)
    # Other units' method definitions, predictions after this unit's own.
    external_methods: dict[str, tuple[ForeignFunction, ...]] = field(
        default_factory=dict
    )
    # A module unit's own import: its initializer belongs to this unit and
    # initializes the unit before running the module body.
    module_import: ImportInfo | None = None
    search_paths: tuple[str, ...] = ()
    toolchain_info: tuple[str, str, str, int] | None = None
    exported_types: dict[tuple[str, str], ValueType] = field(default_factory=dict)
    exported_targets: dict[tuple[str, str], object] = field(default_factory=dict)
    storage_methods: dict[tuple[str, str], ValueType] = field(default_factory=dict)
    builtin_return_types: dict[str, ValueType] = field(default_factory=dict)
    builtin_targets: dict[str, object] = field(default_factory=dict)
    interface_bytes: bytes | None = None


# Every protocol (``$mule.*`` compilation namespaces) runs under the one
# protocol globals namespace, so their cells are keyed by it.
PROTOCOL_NAMESPACE = "$mule.protocols"

_BORROWED_SEQUENCE_PROTOCOLS: frozenset[str] = frozenset()


def runtime_namespace(namespace: str) -> str:
    """The runtime namespace code compiled under ``namespace`` runs in."""
    return PROTOCOL_NAMESPACE if namespace.startswith("$") else namespace


def _symbol(name: str) -> str:
    return '@"' + name + '"'


def _suffixed(symbol: str, suffix: str) -> str:
    """``symbol`` with ``suffix`` appended to its name."""
    if symbol.endswith('"'):
        return symbol[:-1] + suffix + '"'
    return symbol + suffix


def unit_initializer(unit: str) -> str:
    """A unit's startup function: it builds the unit's constants."""
    return _symbol(f"mule.unit:{unit}.initialize")


def unit_finalizer(unit: str) -> str:
    """A unit's exit function: it releases the unit's constants and modules."""
    return _symbol(f"mule.unit:{unit}.finalize")


class _Symbols:
    """One kind of symbol for every lambda a unit may reference. A reference
    to another unit's lambda is recorded so the unit declares it."""

    def __init__(self, owned: set, foreign: dict, suffix: str):
        self.owned = owned
        self.foreign = foreign
        self.suffix = suffix

    def __getitem__(self, lambda_: Function) -> str:
        name = f"{lambda_.name}.{self.suffix}"
        if lambda_ not in self.owned:
            self.foreign[name] = (self.suffix, lambda_)
        return _symbol(name)


def _declared_protocol_context(function: Function, facts):
    """The retained entry accepting the entire declared protocol domain."""
    parameters = function.entry_parameter_types
    if parameters is None or facts is None:
        return None
    for context in facts.variants:
        if context.id == "fixnum" and context.parameters == parameters:
            return context.id
    raise ValueError(f"{function.name}: missing declared protocol entry {parameters!r}")


class _SpecializedSymbols:
    """The guarded entry of a lambda for one of its entry contexts."""

    def __init__(self, owned: set, foreign: dict, facts: dict, backend_contexts: dict):
        self.owned = owned
        self.foreign = foreign
        self.facts = facts
        self.backend_contexts = backend_contexts
        self.requested: set[tuple[Function, object]] = set()

    def __getitem__(self, key: tuple[Function, object]) -> str:
        lambda_, context_id = key
        self.requested.add(key)
        backend = self.backend_contexts.get(key)
        if backend is not None:
            return _symbol(f"{lambda_.name}.{context_id}")
        facts = self.facts.get(lambda_)
        variants = facts.variants if facts is not None else ()
        for variant, context in enumerate(variants):
            if context.id == context_id:
                name = (
                    f"{lambda_.name}.protocol"
                    if context_id == _declared_protocol_context(lambda_, facts)
                    else f"{lambda_.name}.spec.{variant}"
                )
                if lambda_ not in self.owned:
                    self.foreign[name] = ("body", lambda_)
                return _symbol(name)
        raise KeyError((lambda_.name, context_id))


class _UnpackedSymbols:
    """The unpacked tuple-returning entry of a lambda for its general or guarded context."""

    def __init__(self, owned: set, facts: dict):
        self.owned = owned
        self.facts = facts
        self.requested: set[tuple[Function, object]] = set()

    def __getitem__(self, key: tuple[Function, object]) -> str:
        lambda_, context_id = key
        if lambda_ not in self.owned:
            raise KeyError((lambda_.name, context_id))
        self.requested.add(key)
        if context_id is None:
            return _symbol(f"{lambda_.name}.body.unpacked")
        facts = self.facts.get(lambda_)
        variants = facts.variants if facts is not None else ()
        for variant, context in enumerate(variants):
            if context.id == context_id:
                return _symbol(f"{lambda_.name}.spec.{variant}.unpacked")
        raise KeyError((lambda_.name, context_id))


def compile_component(
    lambda_: Lambda, context, *, select_calls=None, emit_coercions: bool = True
):
    """Run the IR2 phases for one body (``compile-component`` for a backend
    whose register allocation LLVM performs)."""
    with specialized_ir1_context(lambda_, context):
        if context.escape_facts is None:
            context.escape_facts = analyze_escapes(
                [lambda_],
                local_environments=context.local_environments,
                base_facts=context.base_escape_facts,
            )
        with phase("LTN %s context %s", lambda_.name, context.id, level=logging.DEBUG):
            ltn_token = ltn_analyze(lambda_, context)
        with phase(
            "IR2 conversion %s context %s",
            lambda_.name,
            context.id,
            level=logging.DEBUG,
        ):
            component = ir2_convert(lambda_, context, ltn_token=ltn_token)
            replace_local_aggregates(component)
        with phase(
            "representation selection %s context %s",
            lambda_.name,
            context.id,
            level=logging.DEBUG,
        ):
            assign_variable_primitive_types(component)
            if select_calls is not None and not str(context.id or "").startswith(
                "fallback.final."
            ):
                select_calls(component)
            select_representations(component)
            if emit_coercions:
                emit_moves_and_coercions(component)
                _specialize_list_getitems(component)
        return component


class _ConstantKey(ast.Visitor):
    """Pool equality preserves scalar types and exact floating-point bits."""

    def visit_Node(self, node):
        raise TypeError("unsupported constant-pool element")

    def visit_NoneLiteral(self, node):
        return ("none",)

    def visit_BooleanLiteral(self, node):
        return "bool", node.value

    def visit_IntegerLiteral(self, node):
        return "int", node.value

    def visit_FloatLiteral(self, node):
        return "float", struct.pack(">d", node.value)

    def visit_TextLiteral(self, node):
        return "text", node.value

    def visit_BytesLiteral(self, node):
        return "bytes", node.value

    def visit_Tuple(self, node):
        return "tuple", tuple(element.accept(self) for element in node.elements)


_CONSTANT_KEY = _ConstantKey()


class _NestedTuples(ast.Visitor):
    """Registers the tuple constants nested in a tuple constant first."""

    def __init__(self, module):
        self.module = module

    def visit_Tuple(self, node):
        self.module.constant_tuple(node)


class _ModuleCollector(NodeVisitor):
    """Collects native method predictions and imported modules in first-use order."""

    def __init__(
        self, method_targets: dict[str, list[Function]], imports: dict[str, object]
    ):
        self.method_targets = method_targets
        self.imports = imports
        self.classes: dict[str, dict[str, Lambda]] = {}

    def collect(self, lambda_: Lambda) -> None:
        self.classes.clear()
        for block in linked(lambda_.component, lambda_, "component").blocks():
            for node in block.nodes():
                node.accept(self)
        for methods in self.classes.values():
            for method_name, target in methods.items():
                targets = self.method_targets.setdefault(method_name, [])
                if target not in targets:
                    targets.append(target)

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return
        name = linked(node.fun_info, node, "known function").name
        if name == "%class_store":
            value = node.args[1]
            target = made_function(value.uses[0]) if len(value.uses) == 1 else None
            if target is not None:
                self.classes.setdefault(node.info.identity, {})[node.info.name] = target
            else:
                self.classes.get(node.info.identity, {}).pop(node.info.name, None)
        elif name == "%class_delete":
            self.classes.get(node.info.identity, {}).pop(node.info.name, None)
        elif name == "%import":
            info = linked(node.info, node, "import")
            self.imports.setdefault(info.module_name, info)


@dataclass(frozen=True, slots=True)
class TupleConstant:
    literal: ast.Tuple
    slot: str
    entry: str
    borrowed_entry: str = ""


class _CallTargetCollector(NodeVisitor):
    def __init__(self, owned: set[Lambda], method_targets: dict[str, set[Lambda]]):
        self.owned = owned
        self.method_targets = method_targets
        self.callees: set[Lambda] = set()
        self.may_call_unknown = False

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN:
            name = linked(node.fun_info, node, "known function").name
            if name == "%resolve_method":
                if node.info is not None:
                    self.callees.update(
                        target
                        for target in self.method_targets.get(node.info.name, ())
                        if target in self.owned
                    )
            elif name == "%call_resolved":
                if node.predicted is not None and node.predicted in self.owned:
                    self.callees.add(node.predicted)
                self.may_call_unknown = True
            elif name in (
                "%current_frame",
                "%current_globals",
                "%eval_source",
                "%exec_source",
                "%locals_dict",
            ):
                self.may_call_unknown = True
            return
        if node.predicted is not None:
            if node.predicted in self.owned:
                self.callees.add(node.predicted)
            else:
                self.may_call_unknown = True
            return
        self.may_call_unknown = True


def _needs_cleanup_release(tn) -> bool:
    if tn.sc is not DESCRIPTOR or tn.borrowed_from is not None:
        return False
    fact = tn.type or UNKNOWN
    return not (fact.is_fixnum or fact.is_immortal)


def _vop_reads_tn(vop, tn) -> bool:
    if any(ref.tn is tn for ref in vop.args):
        return True
    if not any(ref.tn is tn for ref in vop.results):
        return False
    if vop.info is vops.UNBIND_LOCAL:
        return vop.codegen_info is not False
    if vop.info is vops.MOVE:
        return vop.codegen_info is not False and tn.sc.owned
    return False


def _vop_clears_tn(vop, tn) -> bool:
    if vop.info is vops.UNBIND_LOCAL:
        return vop.results[0].tn is tn
    if vop.info is vops.LOAD_LOCAL:
        return vop.args[0].tn is tn and vop.codegen_info.move
    if tn.leaf is not None or (tn.name is not None and tn.name.startswith("$scalar.")):
        return False
    if not any(ref.tn is tn for ref in vop.args):
        return False
    if vop.info.name == "unpack-ref" and vop.args[0].tn is tn:
        return not vop.codegen_info.borrow_parent
    return not (vop.info.name == "sequence-available" and vop.args[0].tn is tn)


def _pack_group_tns(
    candidates,
    fixed,
    prefix: str,
    owned_set: set,
    first_step: dict,
    tn_full_blocks: dict[object, int],
    tn_part_blocks: dict[object, int],
    tn_spans: dict[object, dict[int, tuple[int, int]]],
) -> list[list]:
    groups: list[list] = []
    for tn in fixed:
        tn.slot = f"{prefix}.{len(groups)}"
        groups.append([tn])
    if not candidates:
        return groups
    by_kind: dict[tuple, list] = {}
    for tn in candidates:
        if tn not in first_step:
            continue
        key = (
            tn.sc,
            tn in owned_set,
            tn.private_box,
            (tn.type or UNKNOWN).is_heap_object,
        )
        by_kind.setdefault(key, []).append(tn)
    for kind_tns in by_kind.values():
        kind_tns.sort(key=lambda tn: first_step[tn])
        slots: list[list] = []
        for tn in kind_tns:
            full = tn_full_blocks.get(tn, 0)
            part = tn_part_blocks.get(tn, 0)
            all_b = full | part
            spans = tn_spans.get(tn)
            chosen = None
            for slot in slots:
                s_full = slot[1]
                s_all = slot[2]
                if (all_b & s_full) or (full & s_all):
                    continue
                common = part & s_all
                if common and spans is not None:
                    s_spans = slot[3]
                    conflict = False
                    for b_num, (start, end) in spans.items():
                        if not ((common >> b_num) & 1):
                            continue
                        for o_start, o_end in s_spans.get(b_num, ()):
                            if o_start <= end and start <= o_end:
                                conflict = True
                                break
                        if conflict:
                            break
                    if conflict:
                        continue
                chosen = slot
                break
            if chosen is not None:
                group_idx = chosen[0]
                groups[group_idx].append(tn)
                chosen[1] |= full
                chosen[2] |= all_b
                if spans is not None:
                    s_spans = chosen[3]
                    for b_num, span in spans.items():
                        s_spans.setdefault(b_num, []).append(span)
            else:
                group_idx = len(groups)
                groups.append([tn])
                s_spans = (
                    {b_num: [span] for b_num, span in spans.items()}
                    if spans is not None
                    else {}
                )
                slots.append([group_idx, full, all_b, s_spans])
            tn.slot = f"{prefix}.{group_idx}"
    for tn in candidates:
        if tn not in first_step:
            group_idx = len(groups)
            groups.append([tn])
            tn.slot = f"{prefix}.{group_idx}"
    return groups


def _lifetime_analyze_and_pack_slots(
    component, fixed_locals, packable_locals, value_tns
) -> tuple[list[list], list[list], dict[str, list[list]]]:
    """Analyze cross-block liveness and slot occupancy once (following SBCL
    ``life.lisp``/``pack.lisp``), pack local and temporary slots, and compute
    per-handler abandoned slot groups."""
    candidates = [*packable_locals, *value_tns]
    if not candidates:
        local_groups = _pack_group_tns(
            (), fixed_locals, "%local", set(), {}, {}, {}, {}
        )
        return local_groups, [], {}
    candidate_set = set(candidates)
    owned_set = {tn for tn in candidates if _needs_cleanup_release(tn)}
    blocks = component.blocks
    ir2_blocks = component.ir2_blocks
    block_start = {}
    block_end = {}
    block_first = {}
    block_last = {}
    normal_succs = {}
    handlers = {}
    live_use = {}
    live_kill = {}
    occ_gen = {}
    occ_kill = {}
    occ_mid = {}
    first_step = {}
    step = 0
    for block in blocks:
        start_step = step
        b_first = {}
        b_last = {}
        used, defined = set(), set()
        gen, kill, mid = set(), set(), set()
        for vop in block.vops:
            touched = {
                ref.tn for ref in (*vop.args, *vop.results) if ref.tn in candidate_set
            }
            for tn in touched:
                if tn not in first_step:
                    first_step[tn] = step
                if tn not in b_first:
                    b_first[tn] = step
                b_last[tn] = step
                if tn not in defined and _vop_reads_tn(vop, tn):
                    used.add(tn)
                if any(ref.tn is tn for ref in vop.results):
                    defined.add(tn)
                if tn in owned_set:
                    if _vop_clears_tn(vop, tn):
                        gen.discard(tn)
                        kill.add(tn)
                    if (
                        any(ref.tn is tn for ref in vop.results)
                        and vop.info is not vops.UNBIND_LOCAL
                    ):
                        kill.discard(tn)
                        gen.add(tn)
                        mid.add(tn)
            step += 1
        if step == start_step:
            step += 1
        block_start[block] = start_step
        block_end[block] = step - 1
        block_first[block] = b_first
        block_last[block] = b_last
        live_use[block] = used
        live_kill[block] = defined
        occ_gen[block] = gen
        occ_kill[block] = kill
        occ_mid[block] = mid
        normal_succs[block] = [
            ir2_blocks[succ] for succ in block.block.succ if succ in ir2_blocks
        ]
        handlers[block] = ir2_blocks.get(block.exception_target)
    live_in = {block: set(live_use[block]) for block in blocks}
    live_out = {block: set() for block in blocks}
    changed = True
    while changed:
        changed = False
        for block in reversed(blocks):
            out = set()
            for succ in normal_succs[block]:
                out |= live_in[succ]
            incoming = live_use[block] | (out - live_kill[block])
            handler = handlers[block]
            if handler is not None:
                out |= live_in[handler]
                incoming |= live_in[handler]
            if out != live_out[block] or incoming != live_in[block]:
                live_out[block] = out
                live_in[block] = incoming
                changed = True
    occ_in = {block: set() for block in blocks}
    occ_out = {block: set(occ_gen[block]) for block in blocks}
    changed = True
    while changed:
        changed = False
        for block in blocks:
            outgoing = (occ_in[block] - occ_kill[block]) | occ_gen[block]
            if outgoing != occ_out[block]:
                occ_out[block] = outgoing
                changed = True
            for succ in normal_succs[block]:
                if not outgoing.issubset(occ_in[succ]):
                    occ_in[succ] |= outgoing
                    changed = True
            handler = handlers[block]
            if handler is not None:
                into_handler = (occ_in[block] | occ_mid[block]) & live_in[handler]
                if not into_handler.issubset(occ_in[handler]):
                    occ_in[handler] |= into_handler
                    changed = True
    tn_full_blocks: dict[object, int] = {}
    tn_part_blocks: dict[object, int] = {}
    tn_spans: dict[object, dict[int, tuple[int, int]]] = {}
    for b_num, block in enumerate(blocks):
        b_bit = 1 << b_num
        b_start = block_start[block]
        b_end = block_end[block]
        b_first = block_first[block]
        b_last = block_last[block]
        in_set = live_in[block] | occ_in[block]
        out_set = live_out[block] | occ_out[block]
        for tn in in_set | out_set | b_first.keys():
            s = b_start if tn in in_set else b_first[tn]
            e = b_end if tn in out_set else b_last[tn]
            if tn not in first_step or s < first_step[tn]:
                first_step[tn] = s
            if s <= b_start and e >= b_end:
                tn_full_blocks[tn] = tn_full_blocks.get(tn, 0) | b_bit
            else:
                tn_part_blocks[tn] = tn_part_blocks.get(tn, 0) | b_bit
                tn_spans.setdefault(tn, {})[b_num] = (s, e)
    local_groups = _pack_group_tns(
        packable_locals,
        fixed_locals,
        "%local",
        owned_set,
        first_step,
        tn_full_blocks,
        tn_part_blocks,
        tn_spans,
    )
    value_groups = _pack_group_tns(
        value_tns,
        (),
        "%value",
        owned_set,
        first_step,
        tn_full_blocks,
        tn_part_blocks,
        tn_spans,
    )
    unwind_slot_groups = [
        *local_groups[len(fixed_locals) :],
        *value_groups,
    ]
    unwind_groups_by_target: dict[str, list[list]] = {}
    for target in dict.fromkeys(block.exception_target for block in blocks):
        if target is None or target not in ir2_blocks:
            continue
        handler = ir2_blocks[target]
        abandoned = set()
        for block in blocks:
            if block.exception_target is target:
                abandoned |= occ_in[block] | occ_mid[block]
        abandoned -= live_in[handler]
        if not abandoned:
            continue
        groups = [
            group
            for group in unwind_slot_groups
            if any(member in abandoned for member in group)
        ]
        if groups:
            unwind_groups_by_target[handler.label] = groups
    return local_groups, value_groups, unwind_groups_by_target


class _HelperBuilder:
    """Line and SSA temporary emitter for shared module-level call helpers."""

    def __init__(self, module=None, *, failure_result="ptr null") -> None:
        self.lines: list[str] = []
        self.serial = 0
        # The module generator, for guards that name module-level data.
        self.module = module
        self.failure_result = failure_result
        self.in_cold_block = False

    @contextmanager
    def cold_block(self):
        previous = self.in_cold_block
        self.in_cold_block = True
        try:
            yield
        finally:
            self.in_cold_block = previous

    def line(self, text: str) -> None:
        stripped = text.lstrip()
        if (
            self.in_cold_block
            and stripped.startswith("call ")
            and "@llvm." not in stripped
            and not text.endswith(" cold noinline")
        ):
            text += " cold noinline"
        self.lines.append(text if text.startswith("  ") else "  " + text)

    def operation(self, text: str) -> str:
        if (
            self.in_cold_block
            and text.startswith("call ")
            and "@llvm." not in text
            and not text.endswith(" cold noinline")
        ):
            text += " cold noinline"
        self.serial += 1
        value = f"%t.{self.serial}"
        self.lines.append(f"  {value} = {text}")
        return value

    def fresh(self, prefix="t"):
        self.serial += 1
        return f"%{prefix}.{self.serial}"

    def current_block(self):
        return next(line[:-1] for line in reversed(self.lines) if line.endswith(":"))

    def propagate(self):
        self.line(f"ret {self.failure_result}")

    def check_condition(self, condition):
        rejected = self.fresh("check.failed")[1:]
        accepted = self.fresh("check.accepted")[1:]
        self.line(
            f"br i1 {condition}, label %{rejected}, label %{accepted}, {vops.COLD_BRANCH}"
        )
        self.lines.append(rejected + ":")
        self.propagate()
        self.lines.append(accepted + ":")

    def check_pointer(self, value):
        self.check_condition(self.operation(f"icmp eq ptr {value}, null"))

    def check_status(self, value):
        self.check_condition(self.operation(f"icmp slt i32 {value}, 0"))


class _ArgumentGuard(ValueTypeVisitor):
    """Emit the entry check selected by the parameter fact's own behavior."""

    def __init__(self, gen, value: str):
        self.gen = gen
        self.value = value

    def visit_ValueType(self, fact: ValueType) -> str:
        raise ValueError(f"Unsupported specialization parameter: {fact!r}")

    def visit_UnknownType(self, fact: ValueType) -> str:
        return "true"

    def visit_FunctionType(self, fact) -> str:
        target = fact.callable_target()
        if target is None:
            return self.visit_ValueType(fact)
        slot = self.gen.operation("alloca ptr")
        entry = self.gen.operation(
            f"call ptr @mule_method_entry(ptr {self.value}, ptr {slot})"
        )
        expected = self.gen.module.functions[target]
        return self.gen.operation(f"icmp eq ptr {entry}, {expected}")

    def visit_IntegerType(self, fact: ValueType) -> str:
        if not fact.is_fixnum:
            return self.visit_ValueType(fact)
        operation = self.gen.operation
        bits = operation(f"ptrtoint ptr {self.value} to i64")
        tag = operation(f"and i64 {bits}, 1")
        valid = operation(f"icmp ne i64 {tag}, 0")
        if fact.minimum > FIXNUM_MIN or fact.maximum < FIXNUM_MAX:
            decoded = operation(f"ashr i64 {bits}, 1")
            if fact.minimum > FIXNUM_MIN:
                lower = operation(f"icmp sge i64 {decoded}, {fact.minimum}")
                valid = operation(f"and i1 {valid}, {lower}")
            if fact.maximum < FIXNUM_MAX:
                upper = operation(f"icmp sle i64 {decoded}, {fact.maximum}")
                valid = operation(f"and i1 {valid}, {upper}")
        return valid

    def visit_TextType(self, fact: TextType) -> str:
        if fact.literal is not None or fact.interned:
            return self.visit_ValueType(fact)
        matched = self.gen.operation(f"call i32 @mule_exact_text(ptr {self.value})")
        return self.gen.operation(f"icmp ne i32 {matched}, 0")

    def visit_FloatType(self, fact: ValueType) -> str:
        if fact.may_be_unbound:
            return self.visit_ValueType(fact)
        kind = self.gen.operation(f"call i32 @mule_value_kind(ptr {self.value})")
        return self.gen.operation(
            f"icmp eq i32 {kind}, {int(vops._ReturnValueKind.FLOAT)}"
        )

    def visit_BytesType(self, fact: ValueType) -> str:
        if fact.may_be_unbound:
            return self.visit_ValueType(fact)
        kind = self.gen.operation(f"call i32 @mule_value_kind(ptr {self.value})")
        return self.gen.operation(
            f"icmp eq i32 {kind}, {int(vops._ReturnValueKind.BYTES)}"
        )

    def visit_ByteArrayType(self, fact: ValueType) -> str:
        if fact.may_be_unbound:
            return self.visit_ValueType(fact)
        layout = self.gen.operation(f"call i64 @mule.attr.layout(ptr {self.value})")
        expected = self.gen.operation(
            f"load i64, ptr {self.gen.module.class_layout_slot('mule_sequences_bytearray')}"
        )
        known = self.gen.operation(f"icmp ne i64 {expected}, 0")
        same = self.gen.operation(f"icmp eq i64 {layout}, {expected}")
        return self.gen.operation(f"and i1 {known}, {same}")

    def visit_ListType(self, fact: ListType) -> str:
        if fact.may_be_unbound:
            return self.visit_ValueType(fact)
        matched = self.gen.operation(f"call i32 @mule_exact_list(ptr {self.value})")
        valid = self.gen.operation(f"icmp ne i32 {matched}, 0")
        if fact.element is UNKNOWN:
            return valid
        start = self.gen.fresh("guard.list.start")[1:]
        header = self.gen.fresh("guard.list.header")[1:]
        loop = self.gen.fresh("guard.list.loop")[1:]
        body = self.gen.fresh("guard.list.body")[1:]
        step = self.gen.fresh("guard.list.step")[1:]
        done = self.gen.fresh("guard.list.done")[1:]
        self.gen.line(f"br label %{start}")
        self.gen.lines.append(start + ":")
        self.gen.line(f"br i1 {valid}, label %{header}, label %{done}")
        self.gen.lines.append(header + ":")
        len_addr = self.gen.operation(
            f"getelementptr i64, ptr {self.value}, i64 {vops.VALUE_BUFFER_LENGTH_OFFSET // 8}"
        )
        length = self.gen.operation(f"load i64, ptr {len_addr}")
        data_addr = self.gen.operation(
            f"getelementptr ptr, ptr {self.value}, i64 {vops.VALUE_BUFFER_DATA_OFFSET // 8}"
        )
        data = self.gen.operation(f"load ptr, ptr {data_addr}")
        self.gen.line(f"br label %{loop}")
        self.gen.lines.append(loop + ":")
        idx = self.gen.operation(f"phi i64 [0, %{header}], [%next.{step}, %{step}]")
        at_end = self.gen.operation(f"icmp eq i64 {idx}, {length}")
        self.gen.line(f"br i1 {at_end}, label %{done}, label %{body}")
        self.gen.lines.append(body + ":")
        slot = self.gen.operation(f"getelementptr ptr, ptr {data}, i64 {idx}")
        item = self.gen.operation(f"load ptr, ptr {slot}")
        elem_valid = fact.element.accept(_ArgumentGuard(self.gen, item))
        elem_end = self.gen.current_block()
        self.gen.line(f"br i1 {elem_valid}, label %{step}, label %{done}")
        self.gen.lines.append(step + ":")
        self.gen.lines.append(f"  %next.{step} = add i64 {idx}, 1")
        self.gen.line(f"br label %{loop}")
        self.gen.lines.append(done + ":")
        return self.gen.operation(
            f"phi i1 [false, %{start}], [true, %{loop}], [false, %{elem_end}]"
        )

    def visit_DictType(self, fact) -> str:
        if (
            not fact.proven
            or fact.key != UNKNOWN
            or fact.value != UNKNOWN
            or fact.may_be_unbound
        ):
            return self.visit_ValueType(fact)
        kind = self.gen.operation(f"call i32 @mule_value_kind(ptr {self.value})")
        return self.gen.operation(
            f"icmp eq i32 {kind}, {int(vops._ReturnValueKind.DICT)}"
        )

    def visit_TupleType(self, fact) -> str:
        if fact.may_be_unbound:
            return self.visit_ValueType(fact)
        arity = fact.arity
        if arity is None and fact.elements is not None:
            arity = len(fact.elements)
        if arity is None:
            kind = self.gen.operation(f"call i32 @mule_value_kind(ptr {self.value})")
            valid = self.gen.operation(
                f"icmp eq i32 {kind}, {int(vops._ReturnValueKind.TUPLE)}"
            )
        else:
            valid = self.gen.operation(
                f"call i1 @mule.fast.tuple.shape(ptr {self.value}, i64 {arity})"
            )
        if not fact.elements or all(element is UNKNOWN for element in fact.elements):
            return valid
        start = self.gen.fresh("guard.tuple.start")[1:]
        fields = self.gen.fresh("guard.tuple.fields")[1:]
        done = self.gen.fresh("guard.tuple.done")[1:]
        self.gen.line(f"br label %{start}")
        self.gen.lines.append(start + ":")
        self.gen.line(f"br i1 {valid}, label %{fields}, label %{done}")
        self.gen.lines.append(fields + ":")
        checked = "true"
        for index, element in enumerate(fact.elements):
            if element is UNKNOWN:
                continue
            item = self.gen.operation(
                f"call ptr @mule.fast.tuple.borrow(ptr {self.value}, i64 {index})"
            )
            elem_valid = element.accept(_ArgumentGuard(self.gen, item))
            checked = (
                elem_valid
                if checked == "true"
                else self.gen.operation(f"and i1 {checked}, {elem_valid}")
            )
        last = self.gen.current_block()
        self.gen.line(f"br label %{done}")
        self.gen.lines.append(done + ":")
        return self.gen.operation(f"phi i1 [false, %{start}], [{checked}, %{last}]")

    def visit_UnionType(self, fact) -> str:
        if fact.may_be_unbound:
            return self.visit_ValueType(fact)
        combined = "false"
        for member in fact.members:
            valid = member.accept(self)
            combined = (
                valid
                if combined == "false"
                else self.gen.operation(f"or i1 {combined}, {valid}")
            )
        return combined

    def visit_InstanceType(self, fact) -> str:
        if fact.may_be_unbound:
            return self.visit_ValueType(fact)
        helper = (
            "mule_reference_layout_id"
            if fact.fixed_slots
            else "mule_instance_layout_id"
        )
        layout = self.gen.operation(f"call i64 @{helper}(ptr {self.value})")
        expected = self.gen.operation(
            f"load i64, ptr {self.gen.module.class_layout_slot(fact.identity)}"
        )
        known = self.gen.operation(f"icmp ne i64 {expected}, 0")
        same = self.gen.operation(f"icmp eq i64 {layout}, {expected}")
        return self.gen.operation(f"and i1 {known}, {same}")

    def visit_SetType(self, fact) -> str:
        if not fact.proven or fact.element != UNKNOWN or fact.may_be_unbound:
            return self.visit_ValueType(fact)
        return self.visit_InstanceType(fact)

    def visit_NoneType(self, fact: ValueType) -> str:
        return self.gen.operation(f"icmp eq ptr {self.value}, @mule.none")

    def boolean(self) -> tuple[str, str, str]:
        operation = self.gen.operation
        true = operation(f"icmp eq ptr {self.value}, @mule.true")
        false = operation(f"icmp eq ptr {self.value}, @mule.false")
        return true, false, operation(f"or i1 {true}, {false}")

    def visit_BooleanType(self, fact: ValueType) -> str:
        true, false, boolean = self.boolean()
        allows_false = (fact.minimum is None or fact.minimum <= 0) and (
            fact.maximum is None or fact.maximum >= 0
        )
        allows_true = (fact.minimum is None or fact.minimum <= 1) and (
            fact.maximum is None or fact.maximum >= 1
        )
        if allows_false and allows_true:
            return boolean
        if allows_true:
            return true
        if allows_false:
            return false
        return self.gen.operation("icmp eq i1 true, false")

    def visit_ImmortalType(self, fact: ValueType) -> str:
        _, _, boolean = self.boolean()
        none = self.gen.operation(f"icmp eq ptr {self.value}, @mule.none")
        return self.gen.operation(f"or i1 {boolean}, {none}")


class _TypeAssumption(ValueTypeVisitor):
    """Emit the LLVM assumption selected by a derived value type."""

    def __init__(self, gen, value: str):
        self.gen = gen
        self.value = value

    def visit_ValueType(self, fact: ValueType) -> str | None:
        return None

    def visit_IntegerType(self, fact: ValueType) -> str | None:
        if not fact.is_fixnum:
            return None
        operation = self.gen.operation
        if fact.minimum == fact.maximum:
            encoded = ((fact.minimum << 1) | 1) & ((1 << 64) - 1)
            return operation(
                f"icmp eq ptr {self.value}, inttoptr (i64 {encoded} to ptr)"
            )
        bits = operation(f"ptrtoint ptr {self.value} to i64")
        tag = operation(f"and i64 {bits}, 1")
        valid = operation(f"icmp ne i64 {tag}, 0")
        if fact.minimum > FIXNUM_MIN or fact.maximum < FIXNUM_MAX:
            decoded = operation(f"ashr i64 {bits}, 1")
            if fact.minimum == 0 and fact.maximum < FIXNUM_MAX:
                bounds = operation(f"icmp ult i64 {decoded}, {fact.maximum + 1}")
            else:
                bounds = None
                if fact.minimum > FIXNUM_MIN:
                    bounds = operation(f"icmp sge i64 {decoded}, {fact.minimum}")
                if fact.maximum < FIXNUM_MAX:
                    upper = operation(f"icmp sle i64 {decoded}, {fact.maximum}")
                    bounds = (
                        upper
                        if bounds is None
                        else operation(f"and i1 {bounds}, {upper}")
                    )
            valid = operation(f"and i1 {valid}, {bounds}")
        return valid


def _block_local_last_vop(tn, positions, reads, writes):
    """The last vop using a temporary that is written once and read only by
    later vops of the writer's block, or None when it needs a slot."""
    tn_writes = writes.get(tn)
    if tn_writes is None or len(tn_writes) != 1:
        return None
    writer = tn_writes[0][0]
    writer_pos = positions.get(writer)
    if writer_pos is None:
        return None
    writer_block, last_index = writer_pos
    last = writer
    for reader, _ref in reads.get(tn, ()):
        reader_pos = positions.get(reader)
        if (
            reader_pos is None
            or reader_pos[0] is not writer_block
            or reader_pos[1] <= writer_pos[1]
        ):
            return None
        if reader_pos[1] > last_index:
            last = reader
            last_index = reader_pos[1]
    return last
