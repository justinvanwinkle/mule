"""LLVM code generation over IR2, following ``src/compiler/codegen.lisp``.

``generate`` turns one unit (the lambdas of the user's modules, or of the
compiled builtins layer, with their IR1 facts) into one LLVM module. For each
lambda it runs the IR2 phases per entry context (template selection, IR2
conversion, representation selection) and then emits one LLVM function per
body by invoking each VOP's generator in block order.

Units are compiled separately and linked. Every function's symbols derive
from its lambda's name, which derives from its module's name, so a unit's
output does not depend on what else is compiled with it. A unit declares the
functions of other units it calls, and defines the import initializer of
each module it owns as ``@"mule.import:NAME"``.

Ownership: every TN of descriptor storage owns a runtime reference in an
entry-block slot. Consuming a temporary clears its slot before borrowing the
value to a runtime operation; the operation's result is published before its
inputs are released. Every still-live operand therefore remains visible to the
shared error cleanup path, and the handler-edge unwind blocks release the
temporaries before transferring to the handler.

The module also carries SBCL's external entry points as thin wrappers: the
argument-view entry (``@"NAME.fn"``) validates and transfers arguments to the
body (``@"NAME.body"``); guarded entries (``@"NAME.spec.V"``) and native
entries (``@mule.native.N``) are additional bodies of the same lambda.
"""

from __future__ import annotations

import logging
import re
import struct
from dataclasses import dataclass, field, replace
from functools import cache
from typing import TYPE_CHECKING

from mule import ast
from mule.diagnostics import phase

from . import argument_layout, vops
from .callconv import CallConvention
from .ctype import (
    FIXNUM_MAX,
    FIXNUM_MIN,
    NONE_TYPE,
    UNKNOWN,
    ListType,
    TextType,
    ValueType,
    ValueTypeVisitor,
)
from .escape import analyze_escapes
from .ir1util import block_exception_target, made_function
from .ir2tran import ir2_convert
from .locall import specialized_ir1_context
from .ltn import EntryContext, FixnumGuard, ltn_analyze, primitive_type_of
from .node import (
    CallKind,
    CodeInfo,
    FunctionalKind,
    Lambda,
    LeafVisitor,
    NodeVisitor,
    Signature,
    linked,
)
from .represent import (
    can_use_double,
    emit_moves_and_coercions,
    fresh_box_returns,
    mark_private_boxes,
    select_native_plans,
    select_representations,
)
from .traceback_sites import SourceHighlights
from .vop import (
    BOOLEAN,
    BOOLEAN_T,
    BOXED_FLOAT,
    CLASS,
    DESCRIPTOR,
    DOUBLE,
    FLOAT_BOX_LAYOUT,
    IMMEDIATE,
    INSTANCE,
    INT64_BOX_LAYOUT,
    NONE_T,
    NULL,
    RANGE,
    REFERENCE,
    SIGNED,
    TN,
    BoxLayout,
)

if TYPE_CHECKING:
    from .ir1tran import ImportInfo
    from .native_protocol import Layout

OBJECT = vops.OBJECT
logger = logging.getLogger(__name__)

DECLARATIONS = """declare ptr @mule_literal_integer(ptr)
declare ptr @mule_literal_float(double)
declare ptr @mule_literal_text(ptr, i64)
declare ptr @mule_literal_bytes(ptr, i64)
declare ptr @mule_none()
declare ptr @mule_bool(i32)
declare ptr @mule_load_global(ptr, ptr)
declare i32 @mule_store_global(ptr, ptr, ptr)
declare i32 @mule_link_globals(ptr, i64, ptr, ptr, i32)
declare void @mule_traceback_here(ptr, i32, ptr, ptr, i32, ptr)
declare ptr @mule_frame_head_slot()
declare ptr @mule_closure_slots(ptr)
@mule_safepoint_requested = external global i8
declare i32 @mule_safepoint()
declare ptr @mule_namespace()
declare i32 @mule_namespace_bind(ptr, ptr, ptr)
declare i32 @mule_namespace_bound(ptr, ptr)
declare i32 @mule_assertion_failure(ptr)
declare ptr @mule_sequence_list(i64, ptr)
declare ptr @mule_sequence_tuple(i64, ptr)
declare ptr @mule_sequence_tuple_owned(i64, ptr)
declare ptr @mule_lookup_special(ptr, ptr)
declare ptr @mule_lookup_special_including_defaults(ptr, ptr)
declare ptr @mule_render_text(ptr, i32)
declare ptr @mule_text_result(ptr)
declare ptr @mule_text_length(ptr)
declare ptr @mule_text_character(ptr, ptr)
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
declare i32 @mule_native_zero_division()
declare i32 @mule_native_type_error()
declare i32 @mule_native_initializer_error()
declare i32 @mule_native_overflow()
declare ptr @mule_get_attr_cached(ptr, ptr, ptr)
declare i32 @mule_set_attr_cached(ptr, ptr, ptr, ptr)
declare ptr @mule_resolve_method(ptr, ptr, ptr, ptr)
declare ptr @mule_resolve_super_method(ptr, ptr, ptr, ptr, ptr, ptr)
declare ptr @mule_call_resolved(ptr, ptr, i64, ptr)
declare ptr @mule_call_resolved_owned(ptr, ptr, i64, ptr)
declare i32 @mule_check_args(ptr, i32)
declare i32 @mule_assert_integer(ptr, ptr, ptr, ptr)
declare i32 @mule_assert_float(ptr, ptr)
declare i32 @mule_assert_real(ptr, ptr)
declare i32 @mule_value_kind(ptr)
declare i32 @mule_integer_in_bounds(ptr, ptr, ptr)
declare ptr @mule_tuple_borrow_item(ptr, i64)
declare i32 @mule_inferred_return_error(ptr)
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
declare i32 @mule_reraise()
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
declare ptr @mule_storage_append_move(ptr, ptr)
declare ptr @mule_storage_insert(ptr, ptr, ptr)
declare ptr @mule_storage_delete(ptr, ptr, ptr)
declare ptr @mule_storage_pop(ptr)
declare ptr @mule_storage_tuple(ptr)
declare ptr @mule_tuple_storage(ptr)
declare i32 @mule_tuple_shape_matches(ptr, i64)
declare i32 @mule_list_shape_matches(ptr, i64)
declare i32 @mule_exact_list(ptr)
declare nonnull ptr @mule_tuple_slot(ptr, i64)
declare ptr @mule_storage_compatible(ptr, ptr)
declare ptr @mule_integer_storage(ptr)
declare ptr @mule_class_identity(ptr)
declare ptr @mule_class_parent(ptr)
declare ptr @mule_class_is_subclass(ptr, ptr)
declare ptr @mule_raised_class(ptr)
declare ptr @mule_exception_targets(ptr)
declare ptr @mule_raise_pending(ptr)
declare ptr @mule_active_exception()
declare ptr @mule_exception_instance(ptr)
declare ptr @mule_invalid_raise()
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
declare void @mule_module_loaded(ptr, ptr)
declare void @mule_module_failed(ptr)
declare void @mule_register_compiled_module(ptr, ptr, ptr, ptr, ptr, ptr, i64)
declare void @mule_set_image_search_path(ptr, i64)
declare void @mule_set_attribute_lookup(ptr)
declare ptr @mule_attribute_lookup_once(ptr, ptr)
declare ptr @mule_attribute_lookup_or(ptr, ptr, ptr)
declare ptr @mule_getattr_default(ptr, ptr, ptr, ptr)
declare ptr @mule_hasattr(ptr, ptr, ptr)
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
    helper_text = vops.helpers(bootstrap=bootstrap) + "\n\n" + argument_layout.helpers()
    for line in helper_text.splitlines():
        if current_symbol is not None:
            current_lines.append(line)
            if line == "}":
                helper_defs.append((current_symbol, "\n".join(current_lines)))
                current_symbol = None
                current_lines = []
        elif line.startswith("define "):
            current_symbol = line.split("(")[0].split()[-1]
            current_lines = [line]
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
    exported_types: dict[tuple[str, str], ValueType] = field(default_factory=dict)
    exported_targets: dict[tuple[str, str], object] = field(default_factory=dict)
    storage_methods: dict[tuple[str, str], ValueType] = field(default_factory=dict)
    builtin_return_types: dict[str, ValueType] = field(default_factory=dict)
    builtin_targets: dict[str, object] = field(default_factory=dict)


# Every protocol (``$mule.*`` compilation namespaces) runs under the one
# protocol globals namespace, so their cells are keyed by it.
PROTOCOL_NAMESPACE = "$mule.protocols"


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
                name = f"{lambda_.name}.spec.{variant}"
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


class _IsScalarConstant(ast.Visitor):
    """True when a constant tuple element materializes without a fallible call."""

    def visit_Node(self, node):
        return False

    def visit_NoneLiteral(self, node):
        return True

    def visit_BooleanLiteral(self, node):
        return True

    def visit_IntegerLiteral(self, node):
        return True

    def visit_FloatLiteral(self, node):
        return True

    def visit_TextLiteral(self, node):
        return True

    def visit_BytesLiteral(self, node):
        return True


_IS_SCALAR_CONSTANT = _IsScalarConstant()


class _ConstantTupleArity(ast.Visitor):
    """Fixed tuple arity of a literal AST node, or None."""

    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node):
        if all(element.accept(_IS_SCALAR_CONSTANT) for element in node.elements):
            return len(node.elements)
        return None


_CONSTANT_TUPLE_ARITY = _ConstantTupleArity()


class _LeafTupleArity(LeafVisitor):
    """Fixed tuple arity of a leaf, or None."""

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf):
        return leaf.value.accept(_CONSTANT_TUPLE_ARITY)


_LEAF_TUPLE_ARITY = _LeafTupleArity()


class _ReturnTupleArity(NodeVisitor):
    """Fixed tuple arity of a node directly producing a tuple, or None."""

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node):
        if (
            node.kind is CallKind.KNOWN
            and linked(node.fun_info, node, "known function").name == "%build_tuple"
        ):
            return len(node.args)
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(_LEAF_TUPLE_ARITY)


_RETURN_TUPLE_ARITY = _ReturnTupleArity()


def _flows_directly_to_return(use, return_) -> bool:
    """True when ``use`` is the last operation before ``return_`` with no
    intervening cleanup or node."""
    if use.lvar is not return_.result:
        return False
    if use.next is not None:
        return use.next.next is return_
    return (
        use.block is not None
        and use.block.last is use
        and use.block.succ == [return_.block]
        and return_.block.start is not None
        and return_.block.start.next is return_
    )


def _return_tuple_info(lambda_: Lambda) -> tuple[int, frozenset] | None:
    """When every return path of ``lambda_`` constructs a fixed-size tuple of
    arity ``1 <= K <= 8`` and flows directly to ``return_``, return ``(K, uses)``."""
    if lambda_.kind is FunctionalKind.TOPLEVEL:
        return None
    return_ = lambda_.return_
    if return_ is None or not return_.result.uses:
        return None
    uses = tuple(return_.result.uses)
    arity = None
    for use in uses:
        if not _flows_directly_to_return(use, return_):
            return None
        use_arity = use.accept(_RETURN_TUPLE_ARITY)
        if use_arity is None or not (1 <= use_arity <= 8):
            return None
        if arity is None:
            arity = use_arity
        elif arity != use_arity:
            return None
    if arity is None:
        return None
    return arity, frozenset(uses)


def generate(program: Program) -> str:
    """Return a complete LLVM module with a native executable entry point."""
    with phase("LLVM generation"):
        module = ModuleGenerator(program)
        llvm = module.emit()
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug(
            "LLVM module: %d lambdas, %d native entries, %d tuple constants, "
            "%d text/data constants, %d characters, %d lines",
            len(program.lambdas),
            len(module.native_plans),
            len(module.tuple_constants),
            len(module.data),
            len(llvm),
            llvm.count("\n"),
        )
    return llvm


def compile_component(lambda_: Lambda, context, *, select_calls=None):
    """Run the IR2 phases for one body (``compile-component`` for a backend
    whose register allocation LLVM performs)."""
    with specialized_ir1_context(lambda_, context):
        with phase("LTN %s context %s", lambda_.name, context.id, level=logging.DEBUG):
            ltn_analyze(lambda_, context)
        with phase(
            "IR2 conversion %s context %s",
            lambda_.name,
            context.id,
            level=logging.DEBUG,
        ):
            component = ir2_convert(lambda_, context)
        with phase(
            "representation selection %s context %s",
            lambda_.name,
            context.id,
            level=logging.DEBUG,
        ):
            select_representations(component)
            if select_calls is not None and select_calls(component):
                select_representations(component)
            emit_moves_and_coercions(component)
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


def _entry_steps(module, function, sources) -> tuple:
    """The entry contexts a direct call of ``function`` tries in order, each
    with the parameter checks its arguments' static types do not already
    prove, ending in the general body when no context fits statically."""
    sources = tuple(sources)
    contexts = sorted(
        (
            context
            for context in module.variants_of(function)
            if all(
                source.may_dispatch_fit(expected)
                for source, expected in zip(sources, context.parameters, strict=True)
            )
        ),
        key=lambda context: (
            sum(
                0 if expected is not UNKNOWN and source.dispatch_fits(expected) else 1
                for source, expected in zip(sources, context.parameters, strict=True)
            ),
            sum(fact.dispatch_rank() for fact in context.parameters),
        ),
    )
    steps: list[tuple[EntryContext | None, tuple[ValueType | None, ...]]] = []
    for context in contexts:
        needed = tuple(
            None if source.dispatch_fits(expected) else expected
            for source, expected in zip(sources, context.parameters, strict=True)
        )
        steps.append((context, needed))
        if all(expected is None for expected in needed):
            break
    else:
        steps.append((None, ()))
    return tuple(steps)


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
        elif name == "%import":
            info = linked(node.info, node, "import")
            self.imports.setdefault(info.module_name, info)


@dataclass(frozen=True, slots=True)
class TupleConstant:
    literal: ast.Tuple
    slot: str
    entry: str
    borrowed_entry: str = ""


@dataclass(slots=True)
class NumericEntry:
    function: Lambda
    context: EntryContext
    symbol: str


def _numeric_argument_fits(actual, expected) -> bool:
    # Boolean facts can fit integer bounds, but their singleton pointers do
    # not have the tagged fixnum representation that a scalar entry decodes.
    if expected.is_fixnum and not actual.is_fixnum:
        return False
    if (
        primitive_type_of(expected) is BOXED_FLOAT
        and primitive_type_of(actual) is not BOXED_FLOAT
    ):
        return False
    return actual.fits(expected)


class ModuleGenerator:
    def __init__(self, program: Program):
        self.program = program
        lambdas = program.lambdas
        self.owned = set(lambdas)
        self.owned_names = {lambda_.name for lambda_ in lambdas}
        self.facts = {**program.external_facts, **program.facts}
        # Other units' functions this unit refers to, by symbol name.
        self.foreign: dict[str, tuple[str, Function]] = {}
        self.functions = _Symbols(self.owned, self.foreign, "fn")
        self.bodies = _Symbols(self.owned, self.foreign, "body")
        self.backend_contexts: dict[tuple[Lambda, str], EntryContext] = {}
        self.specialized = _SpecializedSymbols(
            self.owned, self.foreign, self.facts, self.backend_contexts
        )
        self.unpacked = _UnpackedSymbols(self.owned, self.facts)
        self.numeric_entries: dict[tuple[Lambda, object], NumericEntry] = {}
        self.rejected_fixnum_entries: dict[Lambda, EntryContext] = {}
        self.fresh_box_summaries: dict[
            tuple[Lambda, object, str], BoxLayout | None
        ] = {}
        self.by_name = {
            lambda_.name: lambda_ for lambda_ in [*program.external, *lambdas]
        }
        self.layouts = program.layouts
        self.sequence_functions = program.sequence_functions
        with phase("native entry selection", level=logging.DEBUG):
            self.native_plans = select_native_plans(lambdas, self.facts, self.layouts)
        self.return_tuple_arities: dict[Lambda, int] = {}
        self.return_tuple_uses: dict[Lambda, frozenset] = {}
        for lambda_ in lambdas:
            if lambda_ not in self.native_plans:
                info = _return_tuple_info(lambda_)
                if info is not None:
                    (
                        self.return_tuple_arities[lambda_],
                        self.return_tuple_uses[lambda_],
                    ) = info
        escapes = analyze_escapes(lambdas)
        self.borrowed_callees = {}
        if not program.bootstrap and not program.exports:
            for function in lambdas:
                if (
                    function is program.entry
                    or function in self.native_plans
                    or function in self.return_tuple_arities
                    or function.is_protocol
                    or function.takes_closure
                    or function.signature.variadic
                    or function.signature.default_parameters
                    or function.method_class is not None
                    # CO_GENERATOR, CO_COROUTINE, CO_ASYNC_GENERATOR.
                    or (function.code is not None and function.code.flags & 0x2A0)
                ):
                    continue
                candidates = frozenset(
                    index
                    for index, var in enumerate(function.vars)
                    if not var.indirect
                    and not var.sets
                    and not escapes.parameter_escapes(function, index)
                )
                if candidates:
                    self.borrowed_callees[function] = candidates
        logger.debug(
            "Backend input: %d lambdas, %d native entries, %d class layouts",
            len(lambdas),
            len(self.native_plans),
            len(self.layouts),
        )
        self.data: dict[bytes, str] = {}
        self.tuple_constants: dict[tuple, TupleConstant] = {}
        self.float_constants: dict[bytes, tuple[str, str]] = {}
        self.text_constants: dict[bytes, tuple[str, str]] = {}
        self.bytes_constants: dict[bytes, tuple[str, str]] = {}
        self.bigint_constants: dict[int, tuple[str, str]] = {}
        self.protocol_helpers: dict[object, str] = {}
        self.auxiliary: list[str] = []
        self.class_layout_slots: set[str] = set()
        # Each runtime namespace's static cells (the runtime's ``GlobalCell``),
        # by name, linked once per namespace.
        self.global_cells: dict[str, dict[str, str]] = {}
        self.code_descriptors: set[str] = set()
        self.source_highlights = SourceHighlights()
        self.closed_variables = {}
        for child in lambdas:
            if child.environment is not None:
                for var in child.environment.closure:
                    self.closed_variables.setdefault(var.home, set()).add(var)
        self.linked_namespaces: set[str] = set()
        # Names whose reads omit the unbound check because the builtins
        # define them, by namespace, and the namespaces code binds names in.
        self.expected_builtins: dict[str, set[str]] = {}
        self.bound_namespaces: set[str] = set()
        # Namespaces whose cells a linkonce_odr initializer uses, which must
        # then be shared definitions too.
        self.shared_namespaces: set[str] = set()
        self.method_targets: dict[str, list[Function]] = {}
        self.imports: dict[str, object] = {}
        if program.module_import is not None:
            self.imports[program.module_import.module_name] = program.module_import
        collector = _ModuleCollector(self.method_targets, self.imports)
        for lambda_ in lambdas:
            collector.collect(lambda_)
        # Other units' methods are predictions too, after this unit's own.
        for method_name, external_targets in program.external_methods.items():
            targets = self.method_targets.setdefault(method_name, [])
            for target in external_targets:
                if target not in targets:
                    targets.append(target)

    def bodies_by_name(self, name: str) -> str:
        return self.bodies[self.by_name[name]]

    def reference(self, lambda_: Function, name: str, kind: str) -> str:
        """The symbol ``name`` of ``lambda_``; another unit's is declared."""
        if lambda_ not in self.owned:
            self.foreign[name] = (kind, lambda_)
        return _symbol(name)

    def variant_index(self, lambda_: Lambda, context_id) -> int:
        for index, context in enumerate(self.variants_of(lambda_)):
            if context.id == context_id:
                return index
        raise KeyError((lambda_.name, context_id))

    def variants_of(self, lambda_: Lambda):
        facts = self.facts.get(lambda_)
        return facts.variants if facts is not None else ()

    def closure_of(self, lambda_: Function) -> bool:
        """Whether the function body takes a closure parameter."""
        return lambda_.takes_closure

    def closure_operand(self, gen, lambda_: Function, callable_value: str) -> list[str]:
        """The closure operand for a direct call to ``lambda_`` through an
        already-guarded callable object, or nothing when it captures nothing."""
        if not self.closure_of(lambda_):
            return []
        closure = gen.operation(
            f"call ptr @mule_function_closure_of(ptr {callable_value})"
        )
        return [f"ptr {closure}"]

    def import_symbol(self, info) -> str:
        """The function returning the imported module. A missing module's is
        private to the unit; a compiled module's belongs to its own unit."""
        if info.missing:
            return _symbol(f"mule.import.missing:{info.module_name}")
        return _symbol(f"mule.import:{info.module_name}")

    def owns_import(self, info) -> bool:
        return info.native or info.missing or info.initializer in self.owned_names

    def cell(self, namespace: str, name: str) -> str:
        """The static cell (the runtime's ``GlobalCell``) of global ``name``
        for code compiled under ``namespace``."""
        namespace = runtime_namespace(namespace)
        cells = self.global_cells.setdefault(namespace, {})
        symbol = cells.get(name)
        if symbol is None:
            symbol = cells[name] = _symbol(f"mule.cell:{namespace}:{name}")
        return symbol

    def load_global(self, var, safety: int, *, speculative=False) -> str:
        """The call reading global ``var``, owned, under ``safety``.

        Safety 3 is exact Python: the namespace's own binding, else the
        builtin's current value. Below that, builtins are assumed never to be
        rebound, so a cell holds a copy of its builtin until the namespace
        binds the name, and reading a name the builtins define needs no
        check. Other names are checked for being unbound except at safety 0,
        where an unbound read yields the undefined object.
        """
        cell = self.cell(var.namespace, var.name)
        if safety == 3 or self.program.bootstrap:
            helper = "peek.live" if speculative else "live"
        elif var.name in self.program.builtin_names:
            helper = "unchecked"
            self.expected_builtins.setdefault(
                runtime_namespace(var.namespace), set()
            ).add(var.name)
        elif safety == 0 or speculative:
            helper = "unchecked"
        else:
            helper = "checked"
        return f"call ptr @mule.global.{helper}(ptr {cell}, ptr {self.text(var.name)})"

    def store_global(self, namespace: str, name: str, value: str) -> str:
        """The call binding global ``name`` to ``value``, borrowed; its status
        is negative on failure."""
        self.bound_namespaces.add(runtime_namespace(namespace))
        cell = self.cell(namespace, name)
        return f"call i32 @mule_store_global(ptr {cell}, ptr {self.text(name)}, ptr {value})"

    def delete_global(self, var) -> str:
        self.bound_namespaces.add(runtime_namespace(var.namespace))
        cell = self.cell(var.namespace, var.name)
        return f"call i32 @mule_delete_global(ptr {cell}, ptr {self.text(var.name)})"

    def link(self, namespace: str, value: str, failed: str, index: int) -> list[str]:
        """Lines linking ``namespace``'s cells into the namespace object
        ``value`` (``null`` for cells that only read builtins), branching to
        ``failed`` when linking fails."""
        namespace = runtime_namespace(namespace)
        self.linked_namespaces.add(namespace)
        self.global_cells.setdefault(namespace, {})
        return [
            f"  %linked.{index} = call i32 {_symbol(f'mule.link:{namespace}')}(ptr {value})",
            f"  %link.failed.{index} = icmp slt i32 %linked.{index}, 0",
            f"  br i1 %link.failed.{index}, label %{failed}, label %linked.ok.{index}",
            f"linked.ok.{index}:",
        ]

    def global_linkage(self) -> list[str]:
        """Each namespace's cells and the function linking them."""
        unlinked = self.global_cells.keys() - self.linked_namespaces
        if unlinked:
            raise AssertionError(f"global references never linked: {sorted(unlinked)}")
        definitions = []
        for namespace, cells in self.global_cells.items():
            shared = namespace in self.shared_namespaces
            linkage = "linkonce_odr" if shared else "internal"
            constant = "linkonce_odr constant" if shared else "private constant"
            definitions.extend(
                f"{symbol} = {linkage} global {{ptr, i64, ptr, ptr}} "
                "{ptr @mule.undefined, i64 0, ptr null, ptr null}"
                for symbol in cells.values()
            )
            # Names read without a check must be installed builtins.
            expected = self.expected_builtins.get(namespace, set())
            groups = [
                (suffix, [name for name in cells if (name in expected) == builtin])
                for suffix, builtin in (("builtins", True), ("names", False))
            ]
            calls = []
            for suffix, names in groups:
                if not names:
                    continue
                name_table = _symbol(f"mule.{suffix}:{namespace}")
                cell_table = _symbol(f"mule.{suffix}.cells:{namespace}")
                name_values = ", ".join(f"ptr {self.text(name)}" for name in names)
                cell_values = ", ".join(f"ptr {cells[name]}" for name in names)
                definitions += [
                    f"{name_table} = {constant} [{len(names)} x ptr] [{name_values}]",
                    f"{cell_table} = {constant} [{len(names)} x ptr] [{cell_values}]",
                ]
                calls.append(
                    f"  %{suffix} = call i32 @mule_link_globals(ptr %namespace, "
                    f"i64 {len(names)}, ptr {name_table}, ptr {cell_table}, "
                    f"i32 {int(suffix == 'builtins')})"
                )
            body = ["entry:"]
            for index, call in enumerate(calls):
                status = call.split(" = ")[0].strip()
                body += [
                    call,
                    f"  %failed.{index} = icmp slt i32 {status}, 0",
                    f"  br i1 %failed.{index}, label %failure, label %next.{index}",
                    f"next.{index}:",
                ]
            body += ["  ret i32 0", "failure:", "  ret i32 -1"]
            definitions.append(
                f"define {linkage} i32 {_symbol(f'mule.link:{namespace}')}(ptr %namespace) {{\n"
                + "\n".join(body)
                + "\n}"
            )
        return definitions

    def code(self, lambda_: Lambda) -> CodeInfo:
        return lambda_.code or CodeInfo(
            lambda_.source_name or lambda_.name, lambda_.name, "<unknown>", 0
        )

    def code_descriptor(self, lambda_: Lambda) -> str:
        """The constant code metadata (the runtime's ``CodeInfo``) that
        traceback entries for ``lambda_`` refer to."""
        symbol = _suffixed(_symbol(lambda_.name), ".code")
        if symbol not in self.code_descriptors:
            code = self.code(lambda_)
            diagnostic = (
                (f'  File "{code.filename}", line {code.firstlineno}, in {code.name}\n')
                .encode("ascii", "backslashreplace")
                .decode("ascii")
            )
            self.code_descriptors.add(symbol)
            self.auxiliary.append(
                f"{symbol} = private constant {{ptr, ptr, ptr, i32, ptr}} "
                f"{{ptr {self.text(code.name)}, ptr {self.text(code.qualname)}, "
                f"ptr {self.text(code.filename)}, i32 {code.firstlineno}, ptr {self.text(diagnostic)}}}"
            )
        return symbol

    def code_metadata(self, lambda_: Lambda) -> ast.Tuple:
        """Immutable Python introspection facts, from the existing IR signature."""
        code = self.code(lambda_)
        signature = lambda_.signature
        parameters = lambda_.parameters
        kinds = signature.kinds
        closed = self.closed_variables.get(lambda_, set())
        positional_kinds = (
            ast.ParameterKind.POSITIONAL_ONLY,
            ast.ParameterKind.POSITIONAL_OR_KEYWORD,
        )
        order = [index for index, kind in enumerate(kinds) if kind in positional_kinds]
        positional = len(order)
        order += [
            index
            for index, kind in enumerate(kinds)
            if kind == ast.ParameterKind.KEYWORD_ONLY
        ]
        keyword_only = len(order) - positional
        order += [
            index
            for index, kind in enumerate(kinds)
            if kind in (ast.ParameterKind.VAR_POSITIONAL, ast.ParameterKind.VAR_KEYWORD)
        ]
        names = [parameters[index] for index in order]
        names += [
            var.name
            for var in lambda_.locals
            if var.name not in parameters
            and not var.name.startswith("$")
            and var not in closed
        ]
        flags = code.flags
        if ast.ParameterKind.VAR_POSITIONAL in kinds:
            flags |= 4
        if ast.ParameterKind.VAR_KEYWORD in kinds:
            flags |= 8
        texts = lambda values: ast.Tuple([ast.TextLiteral(value) for value in values])
        return ast.Tuple(
            [
                ast.TextLiteral(code.name),
                ast.TextLiteral(code.filename),
                ast.TextLiteral(code.qualname),
                ast.IntegerLiteral(code.firstlineno),
                ast.IntegerLiteral(positional),
                ast.IntegerLiteral(kinds.count(ast.ParameterKind.POSITIONAL_ONLY)),
                ast.IntegerLiteral(keyword_only),
                ast.IntegerLiteral(flags),
                texts(names),
                texts(
                    [
                        name
                        for name in parameters
                        if any(var.name == name for var in closed)
                    ]
                    + sorted(
                        var.name
                        for var in closed
                        if var.name not in parameters and not var.name.startswith("$")
                    )
                ),
                texts(
                    var.name
                    for var in lambda_.environment.closure
                    if not var.name.startswith("$")
                )
                if lambda_.environment
                else texts(()),
                ast.IntegerLiteral(
                    sum(
                        kinds[index] in positional_kinds
                        for index in signature.default_parameters
                    )
                ),
                texts(
                    parameters[index]
                    for index in signature.default_parameters
                    if kinds[index] == ast.ParameterKind.KEYWORD_ONLY
                ),
            ]
        )

    def class_layout_slot(self, identity: str) -> str:
        """The word holding the layout address of the class object the
        definition ``identity`` created most recently, or zero before it
        runs. One definition shared by every unit: a class may be typed in
        units other than the one defining it."""
        name = _symbol(f"mule.class.layout.{identity}")
        if identity not in self.class_layout_slots:
            self.class_layout_slots.add(identity)
            self.auxiliary.append(f"{name} = linkonce_odr global i64 0")
        return name

    def attribute_cache(self) -> str:
        name = f"@mule.cache.{len(self.auxiliary)}"
        # One runtime `AttrCache`; compiled sites probe its first words.
        self.auxiliary.append(
            f"{name} = internal global [{vops.ATTR_CACHE_WORDS} x i64] zeroinitializer"
        )
        return name

    def names(self, names: tuple[str, ...]) -> str:
        if not names:
            return "null"
        values = ", ".join(f"ptr {self.text(name)}" for name in names)
        name = f"@mule.names.{len(self.auxiliary)}"
        self.auxiliary.append(
            f"{name} = private constant [{len(names)} x ptr] [{values}]"
        )
        return name

    def literal_data(self, value: bytes) -> str:
        """Produce a NUL-terminated global, retaining embedded NUL bytes."""
        if value not in self.data:
            self.data[value] = f"@mule.data.{len(self.data)}"
        return self.data[value]

    def text(self, value: str) -> str:
        return self.literal_data(value.encode("utf-8"))

    def constant_float(
        self, literal: ast.FloatLiteral, *, borrowed: bool = False
    ) -> str:
        """A unit-owned immutable float, pooled by its exact binary64 bits."""
        key = struct.pack(">d", literal.value)
        existing = self.float_constants.get(key)
        if existing is None:
            index = len(self.float_constants)
            slot = f"@mule.literal.float.slot.{index}"
            entry = (
                f"call ptr @mule.cached.float(ptr {slot}, "
                f"double {vops.double_constant(literal.value)})"
            )
            self.float_constants[key] = slot, entry
            self.auxiliary.append(f"{slot} = internal global ptr null")
        else:
            slot, entry = existing
        if borrowed:
            return (
                f"call ptr @mule.borrowed.float(ptr {slot}, "
                f"double {vops.double_constant(literal.value)})"
            )
        return entry

    def constant_text(self, literal: ast.TextLiteral, *, borrowed: bool = False) -> str:
        """A unit-owned immutable str, pooled by its WTF-8 byte representation."""
        encoded = literal.value.encode("utf-8", "surrogatepass")
        existing = self.text_constants.get(encoded)
        if existing is None:
            index = len(self.text_constants)
            slot = f"@mule.literal.text.slot.{index}"
            data = self.literal_data(encoded)
            entry = f"call ptr @mule.cached.text(ptr {slot}, ptr {data}, i64 {len(encoded)})"
            self.text_constants[encoded] = slot, entry
            self.auxiliary.append(f"{slot} = internal global ptr null")
        else:
            slot, entry = existing
        if borrowed:
            data = self.literal_data(encoded)
            return f"call ptr @mule.borrowed.text(ptr {slot}, ptr {data}, i64 {len(encoded)})"
        return entry

    def constant_bytes(
        self, literal: ast.BytesLiteral, *, borrowed: bool = False
    ) -> str:
        """A unit-owned immutable bytes object, pooled by its byte sequence."""
        value = literal.value
        existing = self.bytes_constants.get(value)
        if existing is None:
            index = len(self.bytes_constants)
            slot = f"@mule.literal.bytes.slot.{index}"
            data = self.literal_data(value)
            entry = (
                f"call ptr @mule.cached.bytes(ptr {slot}, ptr {data}, i64 {len(value)})"
            )
            self.bytes_constants[value] = slot, entry
            self.auxiliary.append(f"{slot} = internal global ptr null")
        else:
            slot, entry = existing
        if borrowed:
            data = self.literal_data(value)
            return f"call ptr @mule.borrowed.bytes(ptr {slot}, ptr {data}, i64 {len(value)})"
        return entry

    def constant_bigint(self, value: int, *, borrowed: bool = False) -> str:
        """A unit-owned integer initialized from its native GMP constant."""
        existing = self.bigint_constants.get(value)
        if existing is None:
            index = len(self.bigint_constants)
            slot = f"@mule.literal.int.slot.{index}"
            magnitude = abs(value)
            limbs = []
            while magnitude:
                limbs.append(magnitude & ((1 << 64) - 1))
                magnitude >>= 64
            count = len(limbs)
            size = -count if value < 0 else count
            words = ", ".join(f"i64 {limb}" for limb in limbs)
            self.auxiliary.extend(
                [
                    f"{slot}.limbs = private constant [{count} x i64] [{words}], align 8",
                    (
                        f"{slot}.value = private constant {{i32, i32, ptr}} "
                        f"{{i32 0, i32 {size}, ptr {slot}.limbs}}, align 8"
                    ),
                ]
            )
            entry = f"call ptr @mule.cached.int(ptr {slot}, ptr {slot}.value)"
            self.bigint_constants[value] = slot, entry
            self.auxiliary.append(f"{slot} = internal global ptr null")
        else:
            slot, entry = existing
        if borrowed:
            return f"call ptr @mule.borrowed.int(ptr {slot}, ptr {slot}.value)"
        return entry

    def tuple_pack_helper(self, count: int) -> str:
        key = f"$tuple.pack.{count}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = f"@mule.cached.tuple.{count}"
        self.protocol_helpers[key] = symbol
        params = ", ".join(["ptr %slot", *(f"ptr %arg.{i}" for i in range(count))])
        stores = "\n".join(
            f"  %addr.{i} = getelementptr [{count} x ptr], ptr %argv, i64 0, i64 {i}\n"
            f"  store ptr %arg.{i}, ptr %addr.{i}"
            for i in range(count)
        )
        self.auxiliary.append(
            f"define internal ptr {symbol}({params}) {{\n"
            "entry:\n"
            f"  %argv = alloca [{count} x ptr]\n"
            f"{stores}\n"
            f"  %tuple = call ptr @mule.cached.tuple(ptr %slot, i64 {count}, ptr %argv)\n"
            "  ret ptr %tuple\n"
            "}"
        )
        return symbol

    def constant_tuple(self, literal: ast.Tuple, *, borrowed: bool = False) -> str:
        """Register dependencies before their parent, sharing only typed values."""
        key = literal.accept(_CONSTANT_KEY)
        existing = self.tuple_constants.get(key)
        if existing is not None:
            return existing.borrowed_entry if borrowed else existing.entry
        nested = _NestedTuples(self)
        for element in literal.elements:
            element.accept(nested)
        index = len(self.tuple_constants)
        slot = f"@mule.literal.slot.{index}"
        elements = literal.elements
        emitter = vops.LiteralEmitter(self)
        operations = [element.accept(emitter) for element in elements]
        self.auxiliary.append(f"{slot} = internal global ptr null")
        if not elements:
            entry = f"call ptr @mule.cached.tuple(ptr {slot}, i64 0, ptr null)"
            borrowed_entry = (
                f"call ptr @mule.borrowed.tuple(ptr {slot}, i64 0, ptr null)"
            )
        elif not any(operation.startswith("call ") for operation in operations):
            items = f"@mule.literal.items.{len(self.auxiliary)}"
            encoded = ", ".join(
                f"ptr inttoptr ({operation.removeprefix('inttoptr ')})"
                if operation.startswith("inttoptr ")
                else f"ptr {operation.split(',')[1].strip().removeprefix('ptr ')}"
                for operation in operations
            )
            self.auxiliary.append(
                f"{items} = private unnamed_addr constant [{len(elements)} x ptr] [{encoded}]"
            )
            entry = f"call ptr @mule.cached.tuple(ptr {slot}, i64 {len(elements)}, ptr {items})"
            borrowed_entry = f"call ptr @mule.borrowed.tuple(ptr {slot}, i64 {len(elements)}, ptr {items})"
        else:
            symbol = f"@mule.literal.tuple.{index}"
            entry = f"call ptr {symbol}()"
            borrowed_entry = (
                f"call ptr @mule.borrowed.tuple.fn(ptr {slot}, ptr {symbol})"
            )
            self.auxiliary.append(self.tuple_constant_builder(symbol, slot, operations))
        constant = TupleConstant(literal, slot, entry, borrowed_entry)
        self.tuple_constants[key] = constant
        return constant.borrowed_entry if borrowed else constant.entry

    def tuple_constant_builder(
        self, symbol: str, slot: str, operations: list[str]
    ) -> str:
        """Construct the cached tuple on first use; main releases initialized roots."""
        packer = self.tuple_pack_helper(len(operations))
        lines = [
            f"define internal ptr {symbol}() {{",
            "entry:",
            f"  %cached = load ptr, ptr {slot}",
            "  %present = icmp ne ptr %cached, null",
            "  br i1 %present, label %ready, label %build",
            "build:",
        ]
        operands = [f"ptr {slot}"]
        for position, operation in enumerate(operations):
            if operation.startswith("inttoptr "):
                operands.append(f"ptr inttoptr ({operation.removeprefix('inttoptr ')})")
            elif not operation.startswith("call "):
                operands.append(
                    f"ptr {operation.split(',')[1].strip().removeprefix('ptr ')}"
                )
            else:
                value = f"%value.{position}"
                lines.append(f"  {value} = {operation}")
                operands.append(f"ptr {value}")
        lines.extend(
            [
                f"  %built = call ptr {packer}({', '.join(operands)})",
                "  ret ptr %built",
                "ready:",
                "  %owned = call ptr @mule_retain(ptr %cached)",
                "  ret ptr %owned",
                "}",
            ]
        )
        return "\n".join(lines)

    def protocol_helper(self, name: str, arity: int) -> str:
        existing = self.protocol_helpers.get(name)
        if existing is not None:
            return existing
        symbol = _symbol(f"mule.protocol.{name}")
        self.protocol_helpers[name] = symbol
        body = self.bodies[self.sequence_functions[name]]
        params = ", ".join(f"ptr %arg.{i}" for i in range(arity))
        operands = ", ".join(["ptr %globals", *(f"ptr %arg.{i}" for i in range(arity))])
        releases = "\n".join(
            f"  call void @mule.fast.release(ptr %arg.{i})" for i in range(arity)
        )
        self.auxiliary.append(
            f"define internal ptr {symbol}({params}) {{\n"
            "entry:\n"
            "  %globals = call ptr @mule_protocol_globals()\n"
            "  %status = call i32 @mule_enter_frame()\n"
            "  %bad = icmp slt i32 %status, 0\n"
            "  br i1 %bad, label %failed, label %entered\n"
            "failed:\n" + (releases + "\n" if releases else "") + "  ret ptr null\n"
            "entered:\n"
            f"  %result = call ptr {body}({operands})\n"
            "  call void @mule_leave_frame()\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def getitem_helper(self) -> str:
        existing = self.protocol_helpers.get("$getitem.fast")
        if existing is not None:
            return existing
        symbol = "@mule.fast.getitem"
        self.protocol_helpers["$getitem.fast"] = symbol
        slow = self.protocol_helper("getitem", 2)
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %sequence, ptr %index) alwaysinline {{\n"
            "entry:\n"
            "  %fast = call ptr @mule_fast_getitem(ptr %sequence, ptr %index)\n"
            "  %matched = icmp ne ptr %fast, null\n"
            "  br i1 %matched, label %hit, label %slow\n"
            "hit:\n"
            "  ret ptr %fast\n"
            "slow:\n"
            "  %owned.seq = call ptr @mule.fast.retain(ptr %sequence)\n"
            "  %owned.idx = call ptr @mule.fast.retain(ptr %index)\n"
            f"  %result = call ptr {slow}(ptr %owned.seq, ptr %owned.idx)\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def getslice_helper(self) -> str:
        existing = self.protocol_helpers.get("$getslice.fast")
        if existing is not None:
            return existing
        symbol = "@mule.fast.getslice"
        self.protocol_helpers["$getslice.fast"] = symbol
        make_slice = self.protocol_helper("make_slice", 3)
        slow = self.protocol_helper("getitem", 2)
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %sequence, ptr %start, ptr %stop, ptr %step) {{\n"
            "entry:\n"
            "  %fast = call ptr @mule_fast_getslice(ptr %sequence, ptr %start, ptr %stop, ptr %step)\n"
            "  %matched = icmp ne ptr %fast, null\n"
            "  br i1 %matched, label %hit, label %slow\n"
            "hit:\n"
            "  ret ptr %fast\n"
            "slow:\n"
            "  %owned.start = call ptr @mule.fast.retain(ptr %start)\n"
            "  %owned.stop = call ptr @mule.fast.retain(ptr %stop)\n"
            "  %owned.step = call ptr @mule.fast.retain(ptr %step)\n"
            f"  %slice = call ptr {make_slice}(ptr %owned.start, ptr %owned.stop, ptr %owned.step)\n"
            "  %bad = icmp eq ptr %slice, null\n"
            "  br i1 %bad, label %failed, label %lookup\n"
            "failed:\n"
            "  ret ptr null\n"
            "lookup:\n"
            "  %owned.seq = call ptr @mule.fast.retain(ptr %sequence)\n"
            f"  %result = call ptr {slow}(ptr %owned.seq, ptr %slice)\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def setitem_helper(self, *, move: bool = False) -> str:
        key = "$setitem.fast.move" if move else "$setitem.fast"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = "@mule.fast.setitem.move" if move else "@mule.fast.setitem"
        self.protocol_helpers[key] = symbol
        slow = self.protocol_helper("setitem", 3)
        fast_fn = "@mule_fast_setitem_move" if move else "@mule_fast_setitem"
        item_setup = (
            "" if move else "  %owned.item = call ptr @mule.fast.retain(ptr %item)\n"
        )
        item_arg = "%item" if move else "%owned.item"
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %sequence, ptr %index, ptr %item) alwaysinline {{\n"
            "entry:\n"
            f"  %matched = call i32 {fast_fn}(ptr %sequence, ptr %index, ptr %item)\n"
            "  %valid = icmp ne i32 %matched, 0\n"
            "  br i1 %valid, label %hit, label %slow\n"
            "hit:\n"
            "  ret ptr @mule.none\n"
            "slow:\n"
            "  %owned.seq = call ptr @mule.fast.retain(ptr %sequence)\n"
            "  %owned.idx = call ptr @mule.fast.retain(ptr %index)\n"
            f"{item_setup}"
            f"  %result = call ptr {slow}(ptr %owned.seq, ptr %owned.idx, ptr {item_arg})\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def iterate_helper(self, *, move: bool = False) -> str:
        key = "$iterate.fast.move" if move else "$iterate.fast"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = "@mule.fast.iterate.move" if move else "@mule.fast.iterate"
        self.protocol_helpers[key] = symbol
        slow = self.protocol_helper("iterate", 1)
        fast_fn = "@mule_fast_iterate_move" if move else "@mule_fast_iterate"
        slow_setup = (
            "" if move else "  %owned = call ptr @mule.fast.retain(ptr %sequence)\n"
        )
        slow_arg = "%sequence" if move else "%owned"
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %sequence) alwaysinline {{\n"
            "entry:\n"
            f"  %fast = call ptr {fast_fn}(ptr %sequence)\n"
            "  %ok = icmp ne ptr %fast, null\n"
            "  br i1 %ok, label %hit, label %slow\n"
            "hit:\n"
            "  ret ptr %fast\n"
            "slow:\n"
            f"{slow_setup}"
            f"  %result = call ptr {slow}(ptr {slow_arg})\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def next_item_helper(self) -> str:
        existing = self.protocol_helpers.get("$next_item.fast")
        if existing is not None:
            return existing
        symbol = "@mule.fast.next_item"
        self.protocol_helpers["$next_item.fast"] = symbol
        slow = self.protocol_helper("next_item", 1)
        self.auxiliary.append(
            f"define internal i32 {symbol}(ptr %iterator, ptr %out) alwaysinline {{\n"
            "entry:\n"
            "  %status = call i32 @mule_fast_next_item(ptr %iterator, ptr %out)\n"
            "  %handled = icmp sge i32 %status, 0\n"
            "  br i1 %handled, label %fast, label %slow\n"
            "fast:\n"
            "  ret i32 %status\n"
            "slow:\n"
            "  %owned = call ptr @mule.fast.retain(ptr %iterator)\n"
            f"  %item = call ptr {slow}(ptr %owned)\n"
            "  store ptr %item, ptr %out\n"
            "  %failed = icmp eq ptr %item, null\n"
            "  %result = select i1 %failed, i32 -1, i32 1\n"
            "  ret i32 %result\n"
            "}"
        )
        return symbol

    def next_unpack_helper(self, arity: int) -> str:
        key = f"$next_unpack.fast.{arity}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = f"@mule.fast.next_unpack.{arity}"
        self.protocol_helpers[key] = symbol
        slow = self.protocol_helper("next_item", 1)
        self.auxiliary.append(
            f"define internal i32 {symbol}(ptr %iterator, ptr %unpack.out, ptr %out) alwaysinline {{\n"
            "entry:\n"
            f"  %status = call i32 @mule_fast_next_unpack(ptr %iterator, i64 {arity}, ptr %unpack.out, ptr %out)\n"
            "  %handled = icmp sge i32 %status, 0\n"
            "  br i1 %handled, label %fast, label %slow\n"
            "fast:\n"
            "  ret i32 %status\n"
            "slow:\n"
            "  %owned = call ptr @mule.fast.retain(ptr %iterator)\n"
            f"  %item = call ptr {slow}(ptr %owned)\n"
            "  store ptr %item, ptr %out\n"
            "  %failed = icmp eq ptr %item, null\n"
            "  %result = select i1 %failed, i32 -1, i32 1\n"
            "  ret i32 %result\n"
            "}"
        )
        return symbol

    def unpack_slots_helper(self, arity: int) -> str:
        key = f"$unpack.slots.{arity}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = f"@mule.fast.unpack.slots.{arity}"
        self.protocol_helpers[key] = symbol
        slow = self.protocol_helper("unpack", 2)
        copies = []
        for idx in range(arity):
            copies.extend(
                [
                    f"  %elem.{idx} = call ptr @mule_tuple_slot(ptr %tuple, i64 {idx})",
                    f"  %slot.{idx} = getelementptr ptr, ptr %out, i64 {idx}",
                    f"  store ptr %elem.{idx}, ptr %slot.{idx}",
                ]
            )
        copies_block = "\n".join(copies)
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %value, ptr %count, ptr %out) alwaysinline {{\n"
            "entry:\n"
            "  %already = icmp eq ptr %value, null\n"
            "  br i1 %already, label %fast.done, label %check\n"
            "check:\n"
            f"  %matched = call i32 @mule_fast_unpack(ptr %value, i64 {arity}, ptr %out)\n"
            "  %valid = icmp ne i32 %matched, 0\n"
            "  br i1 %valid, label %fast.done, label %slow\n"
            "fast.done:\n"
            "  call void @mule.fast.release(ptr %count)\n"
            "  ret ptr @mule.none\n"
            "slow:\n"
            f"  %tuple = call ptr {slow}(ptr %value, ptr %count)\n"
            "  %bad = icmp eq ptr %tuple, null\n"
            "  br i1 %bad, label %failed, label %copy\n"
            "failed:\n"
            "  ret ptr null\n"
            "copy:\n"
            f"{copies_block}\n"
            "  call void @mule.heap.release(ptr %tuple)\n"
            "  ret ptr @mule.none\n"
            "}"
        )
        return symbol

    def unpack_helper(self, arity: int) -> str:
        key = f"$unpack.{arity}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = f"@mule.fast.unpack.{arity}"
        self.protocol_helpers[key] = symbol
        slow = self.protocol_helper("unpack", 2)
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %value, ptr %count) {{\n"
            "entry:\n"
            f"  %matched = call i32 @mule_tuple_shape_matches(ptr %value, i64 {arity})\n"
            "  %valid = icmp ne i32 %matched, 0\n"
            "  br i1 %valid, label %tuple, label %list.check\n"
            "tuple:\n"
            "  call void @mule.fast.release(ptr %count)\n"
            "  ret ptr %value\n"
            "list.check:\n"
            f"  %matched.list = call i32 @mule_list_shape_matches(ptr %value, i64 {arity})\n"
            "  %valid.list = icmp ne i32 %matched.list, 0\n"
            "  br i1 %valid.list, label %list, label %iterable\n"
            "list:\n"
            "  %snapshot = call ptr @mule_storage_tuple(ptr %value)\n"
            "  call void @mule.fast.release(ptr %value)\n"
            "  call void @mule.fast.release(ptr %count)\n"
            "  ret ptr %snapshot\n"
            "iterable:\n"
            f"  %result = call ptr {slow}(ptr %value, ptr %count)\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def compiled_call_helper(self, function: Function, arity: int) -> str:
        key = f"$compiled.{function.name}.{arity}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = _symbol(f"mule.compiled.{function.name}.{arity}")
        self.protocol_helpers[key] = symbol
        body = self.bodies[function]
        params = ", ".join(["ptr %globals", *(f"ptr %arg.{i}" for i in range(arity))])
        releases = "\n".join(
            f"  call void @mule.fast.release(ptr %arg.{i})" for i in range(arity)
        )
        self.auxiliary.append(
            f"define internal ptr {symbol}({params}) {{\n"
            "entry:\n"
            "  %status = call i32 @mule_enter_frame()\n"
            "  %bad = icmp slt i32 %status, 0\n"
            "  br i1 %bad, label %failed, label %entered\n"
            "failed:\n" + (releases + "\n" if releases else "") + "  ret ptr null\n"
            "entered:\n"
            f"  %result = call ptr {body}({params})\n"
            "  call void @mule_leave_frame()\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def positional_call_helper(self, arity: int) -> str:
        key = f"$call.{arity}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = f"@mule.call.{arity}"
        self.protocol_helpers[key] = symbol
        params = ", ".join(["ptr %callee", *(f"ptr %arg.{i}" for i in range(arity))])
        lines = [f"define internal ptr {symbol}({params}) {{", "entry:"]
        if arity:
            lines.append(f"  %argv = alloca [{arity} x ptr]")
            for i in range(arity):
                lines.append(
                    f"  %slot.{i} = getelementptr [{arity} x ptr], ptr %argv, i64 0, i64 {i}"
                )
                lines.append(f"  store ptr %arg.{i}, ptr %slot.{i}")
            buf = "%argv"
        else:
            buf = "null"
        lines.append(
            f"  %result = call ptr @mule_call_owned(ptr %callee, i64 {arity}, ptr {buf})"
        )
        lines.extend(["  ret ptr %result", "}"])
        self.auxiliary.append("\n".join(lines))
        return symbol

    def resolved_call_helper(self, arity: int) -> str:
        key = f"$call.resolved.{arity}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = f"@mule.call.resolved.{arity}"
        self.protocol_helpers[key] = symbol
        params = ", ".join(
            [
                "ptr %callable",
                "ptr %receiver",
                *(f"ptr %arg.{i}" for i in range(arity)),
            ]
        )
        lines = [f"define internal ptr {symbol}({params}) {{", "entry:"]
        if arity:
            lines.append(f"  %argv = alloca [{arity} x ptr]")
            for i in range(arity):
                lines.append(
                    f"  %slot.{i} = getelementptr [{arity} x ptr], ptr %argv, i64 0, i64 {i}"
                )
                lines.append(f"  store ptr %arg.{i}, ptr %slot.{i}")
            buf = "%argv"
        else:
            buf = "null"
        lines.append(
            f"  %result = call ptr @mule_call_resolved_owned(ptr %callable, ptr %receiver, i64 {arity}, ptr {buf})"
        )
        lines.extend(["  ret ptr %result", "}"])
        self.auxiliary.append("\n".join(lines))
        return symbol

    def keyword_call_helper(
        self, keywords: tuple[str | None, ...], *, resolved: bool
    ) -> str:
        key = ("$call.kw", resolved, keywords)
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        index = len(self.protocol_helpers)
        symbol = f"@mule.call.kw.{index}"
        self.protocol_helpers[key] = symbol
        arity = len(keywords)
        params = [
            "ptr %callable",
            *(["ptr %receiver"] if resolved else []),
            *(f"ptr %arg.{i}" for i in range(arity)),
        ]
        positional = [f"%arg.{i}" for i, name in enumerate(keywords) if name is None]
        named = [
            (name, f"%arg.{i}") for i, name in enumerate(keywords) if name is not None
        ]
        ordered = [*positional, *(val for _, val in named)]
        names = self.names(tuple(name for name, _ in named))
        lines = [f"define internal ptr {symbol}({', '.join(params)}) {{", "entry:"]
        if ordered:
            lines.append(f"  %argv = alloca [{len(ordered)} x ptr]")
            for i, val in enumerate(ordered):
                lines.append(
                    f"  %slot.{i} = getelementptr [{len(ordered)} x ptr], ptr %argv, i64 0, i64 {i}"
                )
                lines.append(f"  store ptr {val}, ptr %slot.{i}")
            pos_buf = "%argv"
            kw_buf = f"%slot.{len(positional)}" if named else "null"
            if named and not positional:
                kw_buf = "%argv"
        else:
            pos_buf = "null"
            kw_buf = "null"
        receiver = "%receiver" if resolved else "null"
        lines.append(
            f"  %result = call ptr @mule_call_keywords(ptr %callable, ptr {receiver}, "
            f"i64 {len(positional)}, ptr {pos_buf}, i64 {len(named)}, ptr {names}, ptr {kw_buf})"
        )
        if resolved:
            lines.append("  call void @mule.fast.release(ptr %receiver)")
        for i in range(arity):
            lines.append(f"  call void @mule.fast.release(ptr %arg.{i})")
        lines.extend(["  ret ptr %result", "}"])
        self.auxiliary.append("\n".join(lines))
        return symbol

    def method_call_helper(
        self,
        functions: tuple[Function, ...],
        keywords: tuple[str | None, ...],
        arity: int,
    ) -> str:
        key = ("$method.direct", functions, keywords, arity)
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        index = len(self.protocol_helpers)
        symbol = _symbol(f"mule.method.{functions[0].name}.{arity}.{index}")
        self.protocol_helpers[key] = symbol
        fallback_helper = (
            self.keyword_call_helper(keywords, resolved=True)
            if keywords
            else self.resolved_call_helper(arity)
        )
        params = [
            "ptr %callable",
            "ptr %receiver",
            *(f"ptr %arg.{i}" for i in range(arity)),
        ]
        fallback_args = ", ".join(params)
        chains = []
        for bound, shape in ((True, "bound"), (False, "unbound")):
            candidates = []
            for function in functions:
                order = function.call_order(keywords, arity, receiver=bound)
                if order is not None:
                    candidates.append((function, order))
            chains.append((bound, shape, candidates))
        targets = tuple(
            f"resolve.{shape}" if candidates else "fallback"
            for _bound, shape, candidates in chains
        )
        lines = [
            f"define internal ptr {symbol}({', '.join(params)}) {{",
            "entry:",
            "  %fn.globals.slot = alloca ptr",
            "  %bound = icmp ne ptr %receiver, null",
            f"  br i1 %bound, label %{targets[0]}, label %{targets[1]}",
        ]
        need_failed_block = False

        def emit_invoke(function, order, variant_index, bound):
            nonlocal need_failed_block
            lines.append(f"direct.{variant_index}:")
            available = []
            for pos, idx in enumerate(order):
                if idx < -1:
                    lines.extend(
                        [
                            f"  %def.{variant_index}.{pos} = call ptr @mule_function_default(ptr %callable, i32 {-2 - idx})",
                            f"  %def.available.{variant_index}.{pos} = icmp ne ptr %def.{variant_index}.{pos}, null",
                        ]
                    )
                    available.append(f"%def.available.{variant_index}.{pos}")
            if available:
                ready = available[0]
                for offset, present in enumerate(available[1:]):
                    combined = f"%defaults.available.{variant_index}.{offset}"
                    lines.append(f"  {combined} = and i1 {ready}, {present}")
                    ready = combined
                lines.extend(
                    [
                        f"  br i1 {ready}, label %defaults.ready.{variant_index}, label %fallback",
                        f"defaults.ready.{variant_index}:",
                    ]
                )
            if (
                bound
                and function.name == "mule_sequences_mule_fn_ListStorage.append"
                and order
                == (
                    -1,
                    0,
                )
            ):
                lines.extend(
                    [
                        f"  %append.res.{variant_index} = call ptr @mule_storage_append(ptr %receiver, ptr %arg.0)",
                        "  call void @mule.fast.release(ptr %receiver)",
                        "  call void @mule.fast.release(ptr %arg.0)",
                        f"  ret ptr %append.res.{variant_index}",
                    ]
                )
                return
            if (
                bound
                and function.name == "mule_sequences_mule_fn_ListStorage.pop"
                and order == (-1, -2)
            ):
                lines.extend(
                    [
                        f"  %pop.res.{variant_index} = call ptr @mule_storage_pop(ptr %receiver)",
                        "  call void @mule.fast.release(ptr %receiver)",
                        f"  ret ptr %pop.res.{variant_index}",
                    ]
                )
                return
            need_failed_block = True
            entered_label = f"entered.{variant_index}"
            lines.extend(
                [
                    f"  %frame.{variant_index} = call i32 @mule_enter_frame()",
                    f"  %rejected.{variant_index} = icmp slt i32 %frame.{variant_index}, 0",
                    f"  br i1 %rejected.{variant_index}, label %failed, label %{entered_label}",
                    f"{entered_label}:",
                    f"  %globals.{variant_index} = load ptr, ptr %fn.globals.slot",
                ]
            )
            owned = []
            for pos, idx in enumerate(order):
                if idx == -1:
                    owned.append("%receiver")
                elif idx >= 0:
                    owned.append(f"%arg.{idx}")
                else:
                    lines.append(
                        f"  %own.{variant_index}.{pos} = call ptr @mule.fast.retain(ptr %def.{variant_index}.{pos})"
                    )
                    owned.append(f"%own.{variant_index}.{pos}")
            closure_ops = []
            if self.closure_of(function):
                lines.append(
                    f"  %closure.{variant_index} = call ptr @mule_function_closure_of(ptr %callable)"
                )
                closure_ops.append(f"ptr %closure.{variant_index}")
            body_args = ", ".join(
                [
                    f"ptr %globals.{variant_index}",
                    *closure_ops,
                    *(f"ptr {v}" for v in owned),
                ]
            )
            lines.extend(
                [
                    f"  %direct.res.{variant_index} = call ptr {self.bodies[function]}({body_args})",
                    "  call void @mule_leave_frame()",
                    f"  ret ptr %direct.res.{variant_index}",
                ]
            )

        for chain_index, (bound, shape, candidates) in enumerate(chains):
            if not candidates:
                continue
            lines.extend(
                [
                    f"resolve.{shape}:",
                    f"  %entry.{shape} = call ptr @mule_method_entry(ptr %callable, ptr %fn.globals.slot)",
                    f"  br label %check.{shape}.0",
                ]
            )
            for candidate_index, (function, order) in enumerate(candidates):
                variant_index = 2 * candidate_index + chain_index
                next_label = (
                    f"check.{shape}.{candidate_index + 1}"
                    if candidate_index + 1 < len(candidates)
                    else "fallback"
                )
                lines.extend(
                    [
                        f"check.{shape}.{candidate_index}:",
                        f"  %match.{shape}.{candidate_index} = icmp eq ptr %entry.{shape}, {self.functions[function]}",
                        f"  br i1 %match.{shape}.{candidate_index}, label %direct.{variant_index}, label %{next_label}",
                    ]
                )
                emit_invoke(function, order, variant_index, bound)
        if need_failed_block:
            lines.extend(
                [
                    "failed:",
                    "  call void @mule.fast.release(ptr %receiver)",
                ]
            )
            for i in range(arity):
                lines.append(f"  call void @mule.fast.release(ptr %arg.{i})")
            lines.append("  ret ptr null")
        lines.extend(
            [
                "fallback:",
                f"  %fallback.res = call ptr {fallback_helper}({fallback_args})",
                "  ret ptr %fallback.res",
                "}",
            ]
        )
        self.auxiliary.append("\n".join(lines))
        return symbol

    def direct_call_helper(
        self,
        function: Function,
        keywords: tuple[str | None, ...],
        arity: int,
        steps: tuple[tuple[EntryContext | None, tuple[ValueType | None, ...]], ...],
        *,
        proven: bool = False,
        receiver_call: bool = False,
        borrowed_arguments: frozenset[int] = frozenset(),
    ) -> str:
        step_key = tuple(
            (None if ctx is None else ctx.id, needed) for ctx, needed in steps
        )
        key = (
            "$call.direct",
            function,
            keywords,
            arity,
            step_key,
            proven,
            receiver_call,
            borrowed_arguments,
        )
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        index = len(self.protocol_helpers)
        symbol = _symbol(f"mule.direct.{function.name}.{arity}.{index}")
        self.protocol_helpers[key] = symbol
        order = function.call_order(keywords, arity)
        params = [
            "ptr %globals",
            "ptr %callee",
            *(f"ptr %arg.{i}" for i in range(arity)),
        ]
        builder = _HelperBuilder(self)
        body_globals = "%callee.globals"
        if proven:
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
                "  %fn.globals.slot = alloca ptr",
                "  %callee.entry = call ptr @mule_method_entry(ptr %callee, ptr %fn.globals.slot)",
                "  %callee.globals = load ptr, ptr %fn.globals.slot",
            ]
        else:
            if receiver_call:
                # The callee was resolved from the receiver (``%arg.0``);
                # any other function takes the bound-call path.
                fallback_helper = self.resolved_call_helper(arity - 1)
                fallback_args = ", ".join(
                    ["ptr %callee", *(f"ptr %arg.{i}" for i in range(arity))]
                )
            else:
                fallback_helper = (
                    self.keyword_call_helper(keywords, resolved=False)
                    if keywords
                    else self.positional_call_helper(arity)
                )
                fallback_args = ", ".join(
                    ["ptr %callee", *(f"ptr %arg.{i}" for i in range(arity))]
                )
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
                "  %fn.globals.slot = alloca ptr",
                "  %callee.entry = call ptr @mule_method_entry(ptr %callee, ptr %fn.globals.slot)",
                f"  %matching = icmp eq ptr %callee.entry, {self.functions[function]}",
            ]
            if receiver_call:
                # Resolution binds a receiver only for a class's method; a
                # function found elsewhere (an instance field) is unbound.
                builder.lines += [
                    "  %bound.receiver = icmp ne ptr %arg.0, null",
                    "  %matching.bound = and i1 %matching, %bound.receiver",
                    "  br i1 %matching.bound, label %direct, label %fallback",
                    "direct:",
                ]
            else:
                builder.lines += [
                    "  br i1 %matching, label %direct, label %fallback",
                    "direct:",
                ]
            builder.line("  %callee.globals = load ptr, ptr %fn.globals.slot")

        def emit_invoke(context: EntryContext | None, tag: int) -> None:
            if borrowed_arguments:
                context = self.borrowed_context(function, context, borrowed_arguments)
            entered = f"entered.{tag}"
            builder.line("  %frame." + str(tag) + " = call i32 @mule_enter_frame()")
            builder.line(f"  %rejected.{tag} = icmp slt i32 %frame.{tag}, 0")
            builder.line(f"  br i1 %rejected.{tag}, label %failed, label %{entered}")
            builder.lines.append(f"{entered}:")
            owned = []
            for pos, idx in enumerate(order):
                if idx >= 0:
                    owned.append(f"%arg.{idx}")
                else:
                    builder.line(
                        f"  %def.{tag}.{pos} = call ptr @mule_function_default(ptr %callee, i32 {-2 - idx})"
                    )
                    builder.line(
                        f"  %own.{tag}.{pos} = call ptr @mule.fast.retain(ptr %def.{tag}.{pos})"
                    )
                    owned.append(f"%own.{tag}.{pos}")
            closure_ops = []
            if self.closure_of(function):
                builder.line(
                    f"  %closure.{tag} = call ptr @mule_function_closure_of(ptr %callee)"
                )
                closure_ops.append(f"ptr %closure.{tag}")
            body = (
                self.specialized[function, context.id]
                if context is not None
                else self.bodies[function]
            )
            body_args = ", ".join(
                [f"ptr {body_globals}", *closure_ops, *(f"ptr {v}" for v in owned)]
            )
            builder.line(f"  %res.{tag} = call ptr {body}({body_args})")
            builder.line("  call void @mule_leave_frame()")
            if context is not None:
                assumed = context.return_type.accept(
                    _TypeAssumption(builder, f"%res.{tag}")
                )
                if assumed is not None:
                    missing = builder.operation(f"icmp eq ptr %res.{tag}, null")
                    assumed = builder.operation(f"or i1 {missing}, {assumed}")
                    builder.line(f"  call void @llvm.assume(i1 {assumed})")
            builder.line(f"  ret ptr %res.{tag}")

        for step_idx, (context, needed) in enumerate(steps):
            guards = [
                expected.accept(_ArgumentGuard(builder, f"%arg.{order[param_idx]}"))
                for param_idx, expected in enumerate(needed)
                if expected is not None
            ]
            if not guards:
                emit_invoke(context, step_idx)
                break
            valid = guards[0]
            for guard in guards[1:]:
                valid = builder.operation(f"and i1 {valid}, {guard}")
            typed = f"typed.{step_idx}"
            next_label = f"next.{step_idx}"
            builder.line(f"  br i1 {valid}, label %{typed}, label %{next_label}")
            builder.lines.append(f"{typed}:")
            emit_invoke(context, step_idx)
            builder.lines.append(f"{next_label}:")

        builder.lines.append("failed:")
        for i in range(arity):
            if i not in borrowed_arguments:
                builder.line(f"  call void @mule.fast.release(ptr %arg.{i})")
        builder.line("  ret ptr null")
        if not proven:
            builder.lines.append("fallback:")
            for index in borrowed_arguments:
                builder.line(f"  call ptr @mule.fast.retain(ptr %arg.{index})")
            builder.lines.extend(
                [
                    f"  %fallback.res = call ptr {fallback_helper}({fallback_args})",
                    "  ret ptr %fallback.res",
                ]
            )
        builder.lines.append("}")
        self.auxiliary.append("\n".join(builder.lines))
        return symbol

    def return_contract_helper(self, contract, *, unpacked_arity=None):
        """Borrow a boxed result or an already-owned tuple result buffer."""
        key = ("$return.contract", contract, unpacked_arity)
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = _symbol(f"mule.return.check.{len(self.protocol_helpers)}")
        self.protocol_helpers[key] = symbol
        builder = _HelperBuilder(self, failure_result="i32 -1")
        builder.lines = [f"define internal i32 {symbol}(ptr %value) {{", "entry:"]
        if unpacked_arity is None:
            contract.accept(vops._ContractAssertion(builder, "%value"))
        else:
            vops.assert_unpacked_return(builder, contract, "%value", unpacked_arity)
        builder.line("ret i32 0")
        builder.lines.append("}")
        self.auxiliary.append("\n".join(builder.lines))
        return symbol

    def unpacked_direct_call_helper(
        self,
        function: Function,
        keywords: tuple[str | None, ...],
        arity: int,
        unpack_arity: int,
        steps: tuple[tuple[EntryContext | None, tuple[ValueType | None, ...]], ...],
        *,
        proven: bool = False,
        result_contract=None,
    ) -> str:
        step_key = tuple(
            (None if ctx is None else ctx.id, needed) for ctx, needed in steps
        )
        key = (
            "$call.direct.unpack",
            function,
            keywords,
            arity,
            unpack_arity,
            step_key,
            proven,
            result_contract,
        )
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        index = len(self.protocol_helpers)
        symbol = _symbol(
            f"mule.direct.unpack.{function.name}.{arity}.{unpack_arity}.{index}"
        )
        self.protocol_helpers[key] = symbol
        order = function.call_order(keywords, arity)
        params = [
            "ptr %globals",
            "ptr %callee",
            *(f"ptr %arg.{i}" for i in range(arity)),
            "ptr %tuple.out",
        ]
        builder = _HelperBuilder(self)
        body_globals = "%callee.globals"
        if proven:
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
                "  %fn.globals.slot = alloca ptr",
                "  %callee.entry = call ptr @mule_method_entry(ptr %callee, ptr %fn.globals.slot)",
                "  %callee.globals = load ptr, ptr %fn.globals.slot",
            ]
        else:
            fallback_helper = (
                self.keyword_call_helper(keywords, resolved=False)
                if keywords
                else self.positional_call_helper(arity)
            )
            fallback_args = ", ".join(
                ["ptr %callee", *(f"ptr %arg.{i}" for i in range(arity))]
            )
            unpack_helper = self.unpack_helper(unpack_arity)
            encoded_arity = (unpack_arity << 1) | 1
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
                "  %fn.globals.slot = alloca ptr",
                "  %callee.entry = call ptr @mule_method_entry(ptr %callee, ptr %fn.globals.slot)",
                f"  %matching = icmp eq ptr %callee.entry, {self.functions[function]}",
                "  br i1 %matching, label %direct, label %fallback",
                "direct:",
                "  %callee.globals = load ptr, ptr %fn.globals.slot",
            ]

        def emit_invoke(context: EntryContext | None, tag: int) -> None:
            entered = f"entered.{tag}"
            builder.line("  %frame." + str(tag) + " = call i32 @mule_enter_frame()")
            builder.line(f"  %rejected.{tag} = icmp slt i32 %frame.{tag}, 0")
            builder.line(f"  br i1 %rejected.{tag}, label %failed, label %{entered}")
            builder.lines.append(f"{entered}:")
            owned = []
            for pos, idx in enumerate(order):
                if idx >= 0:
                    owned.append(f"%arg.{idx}")
                else:
                    builder.line(
                        f"  %def.{tag}.{pos} = call ptr @mule_function_default(ptr %callee, i32 {-2 - idx})"
                    )
                    builder.line(
                        f"  %own.{tag}.{pos} = call ptr @mule.fast.retain(ptr %def.{tag}.{pos})"
                    )
                    owned.append(f"%own.{tag}.{pos}")
            closure_ops = []
            if self.closure_of(function):
                builder.line(
                    f"  %closure.{tag} = call ptr @mule_function_closure_of(ptr %callee)"
                )
                closure_ops.append(f"ptr %closure.{tag}")
            body = self.unpacked[function, None if context is None else context.id]
            body_args = ", ".join(
                [
                    f"ptr {body_globals}",
                    *closure_ops,
                    *(f"ptr {v}" for v in owned),
                    "ptr %tuple.out",
                ]
            )
            builder.line(f"  %res.{tag} = call ptr {body}({body_args})")
            builder.line("  call void @mule_leave_frame()")
            returned = (
                self.facts[function].return_type
                if context is None
                else context.return_type
            )
            if result_contract is not None and not returned.fits(result_contract.fact):
                checker = self.return_contract_helper(
                    result_contract, unpacked_arity=unpack_arity
                )
                builder.line(f"  %null.{tag} = icmp eq ptr %res.{tag}, null")
                builder.line(
                    f"  br i1 %null.{tag}, label %invoke.failed.{tag}, label %invoke.check.{tag}"
                )
                builder.lines.append(f"invoke.failed.{tag}:")
                builder.line("ret ptr null")
                builder.lines.append(f"invoke.check.{tag}:")
                checked = builder.operation(f"call i32 {checker}(ptr %tuple.out)")
                rejected = builder.operation(f"icmp slt i32 {checked}, 0")
                builder.line(
                    f"br i1 {rejected}, label %invoke.incompatible.{tag}, label %invoke.accepted.{tag}"
                )
                builder.lines.append(f"invoke.incompatible.{tag}:")
                for slot_index in range(unpack_arity):
                    slot = builder.operation(
                        f"getelementptr ptr, ptr %tuple.out, i64 {slot_index}"
                    )
                    value = builder.operation(f"load ptr, ptr {slot}")
                    builder.line(f"store ptr null, ptr {slot}")
                    builder.line(f"call void @mule.fast.release(ptr {value})")
                builder.line("ret ptr null")
                builder.lines.append(f"invoke.accepted.{tag}:")
            builder.line(f"  ret ptr %res.{tag}")

        for step_idx, (context, needed) in enumerate(steps):
            guards = [
                expected.accept(_ArgumentGuard(builder, f"%arg.{order[param_idx]}"))
                for param_idx, expected in enumerate(needed)
                if expected is not None
            ]
            if not guards:
                emit_invoke(context, step_idx)
                break
            valid = guards[0]
            for guard in guards[1:]:
                valid = builder.operation(f"and i1 {valid}, {guard}")
            typed = f"typed.{step_idx}"
            next_label = f"next.{step_idx}"
            builder.line(f"  br i1 {valid}, label %{typed}, label %{next_label}")
            builder.lines.append(f"{typed}:")
            emit_invoke(context, step_idx)
            builder.lines.append(f"{next_label}:")

        builder.lines.append("failed:")
        for i in range(arity):
            builder.line(f"  call void @mule.fast.release(ptr %arg.{i})")
        builder.line("  ret ptr null")
        if not proven:
            builder.lines.extend(
                [
                    "fallback:",
                    f"  %fallback.res = call ptr {fallback_helper}({fallback_args})",
                    "  %fallback.null = icmp eq ptr %fallback.res, null",
                    "  br i1 %fallback.null, label %fallback.err, label %fallback.check",
                    "fallback.err:",
                    "  ret ptr null",
                    "fallback.check:",
                ]
            )
            if result_contract is not None:
                checker = self.return_contract_helper(result_contract)
                checked = builder.operation(f"call i32 {checker}(ptr %fallback.res)")
                rejected = builder.operation(f"icmp slt i32 {checked}, 0")
                builder.line(
                    f"br i1 {rejected}, label %fallback.incompatible, label %fallback.unpack"
                )
                builder.lines.append("fallback.incompatible:")
                builder.line("call void @mule.fast.release(ptr %fallback.res)")
                builder.line("ret ptr null")
            else:
                builder.line("br label %fallback.unpack")
            builder.lines.extend(
                [
                    "fallback.unpack:",
                    f"  %unpacked = call ptr {unpack_helper}(ptr %fallback.res, ptr inttoptr (i64 {encoded_arity} to ptr))",
                    "  %unpacked.null = icmp eq ptr %unpacked, null",
                    "  br i1 %unpacked.null, label %fallback.err, label %fallback.slots",
                    "fallback.slots:",
                ]
            )
            for slot_idx in range(unpack_arity):
                builder.lines.extend(
                    [
                        f"  %slot.val.{slot_idx} = call ptr @mule_tuple_slot(ptr %unpacked, i64 {slot_idx})",
                        f"  %slot.addr.{slot_idx} = getelementptr ptr, ptr %tuple.out, i64 {slot_idx}",
                        f"  store ptr %slot.val.{slot_idx}, ptr %slot.addr.{slot_idx}",
                    ]
                )
            builder.lines.extend(
                [
                    "  call void @mule.heap.release(ptr %unpacked)",
                    "  ret ptr @mule.none",
                ]
            )
        builder.lines.append("}")
        self.auxiliary.append("\n".join(builder.lines))
        return symbol

    def contexts(self, lambda_: Lambda):
        """The general entry context and the guarded variants."""
        facts = self.facts[lambda_]
        general = EntryContext(
            None,
            facts.types,
            facts.targets,
            facts.parameters or (UNKNOWN,) * len(lambda_.vars),
            facts.return_type,
            borrowed_callees=self.borrowed_callees,
            element_contracts=facts.element_contracts,
        )
        variants = [
            EntryContext(
                context.id,
                context.types,
                context.targets,
                context.parameters,
                context.return_type,
                borrowed_callees=self.borrowed_callees,
                element_contracts=context.element_contracts,
            )
            for context in facts.variants
        ]
        return general, variants

    def borrowed_context(self, function: Function, base, borrowed: frozenset[int]):
        if function not in self.owned:
            raise ValueError("borrowed contexts require a function owned by this unit")
        context_id = base.id if base is not None else None
        general, variants = self.contexts(function)
        base = (
            general
            if context_id is None
            else next((entry for entry in variants if entry.id == context_id), None)
        )
        if base is None:
            raise ValueError(
                f"{function.name}: no backend entry context {context_id!r}"
            )
        identity = (
            "borrow."
            + str(base.id or "general")
            + "."
            + ".".join(map(str, sorted(borrowed)))
        )
        key = function, identity
        context = self.backend_contexts.get(key)
        if context is None:
            context = replace(base, id=identity, borrowed_parameters=borrowed)
            self.backend_contexts[key] = context
        return context

    def numeric_entry(self, function: Lambda, context: EntryContext) -> NumericEntry:
        """Select an internal ABI without changing the entry's semantic facts."""
        key = function, context.id
        existing = self.numeric_entries.get(key)
        if existing is not None:
            return existing
        returned = (
            DOUBLE if primitive_type_of(context.return_type) is BOXED_FLOAT else SIGNED
        )
        parameters = tuple(
            DOUBLE
            if primitive_type_of(fact) is BOXED_FLOAT
            else SIGNED
            if fact.is_fixnum
            else DESCRIPTOR
            for fact in context.parameters
        )
        numeric = replace(context, convention=CallConvention(parameters, returned))
        # Signature probing does not select calls or retain cloned IR1 nodes.
        # Object-sensitive parameter uses must keep the incoming object itself.
        component = compile_component(function, numeric)
        parameters = tuple(
            DESCRIPTOR
            if sc is DOUBLE
            and not can_use_double(component, component.environment.vars[var])
            else sc
            for var, sc in zip(function.vars, parameters, strict=True)
        )
        numeric.convention = CallConvention(parameters, returned)
        entry = NumericEntry(
            function,
            numeric,
            _symbol(f"{function.name}.numeric.{context.id or 'general'}"),
        )
        self.numeric_entries[key] = entry
        return entry

    def select_numeric_calls(self, component) -> bool:
        """Use scalar locations only for proven calls and compatible consumers."""
        if component.context.plan is not None or component.context.borrowed_parameters:
            return False
        changed = False
        pending = [
            vop
            for block in component.blocks
            for vop in block.vops
            if vop.info is vops.FULL_CALL
        ]
        while pending:
            remaining = []
            progress = False
            for vop in pending:
                site = vop.codegen_info
                # Immediates have no counted ownership to specialize; borrowing
                # one must not prevent an otherwise valid numeric convention.
                if any(
                    ref.tn.borrowed_from is not None and ref.tn.sc.owned
                    for ref in vop.args[1:]
                ):
                    continue
                target = site.target
                if (
                    not site.proven
                    or site.keywords
                    or target not in self.owned
                    or target in self.native_plans
                    or target is component.lambda_
                    or target.call_order((), len(vop.args) - 1)
                    != tuple(range(len(target.vars)))
                ):
                    continue
                result = vop.results[0].tn
                fact = result.type or UNKNOWN
                if primitive_type_of(fact) is BOXED_FLOAT:
                    if not can_use_double(component, result, ignore=(vop,)):
                        remaining.append(vop)
                        continue
                elif not fact.is_fixnum:
                    continue
                arguments = tuple(arg.tn.type or UNKNOWN for arg in vop.args[1:])
                general, variants = self.contexts(target)
                context = next(
                    (
                        candidate
                        for candidate in (*variants, general)
                        if all(
                            _numeric_argument_fits(actual, expected)
                            for actual, expected in zip(
                                arguments, candidate.parameters, strict=True
                            )
                        )
                        and (
                            candidate.return_type.is_fixnum
                            if fact.is_fixnum
                            else primitive_type_of(candidate.return_type) is BOXED_FLOAT
                        )
                        and candidate.return_type.fits(fact)
                    ),
                    None,
                )
                if context is None:
                    continue
                entry = self.numeric_entry(target, context)
                convention = linked(entry.context.convention, entry, "convention")
                restrictions = tuple(
                    (DESCRIPTOR, DOUBLE)
                    if sc is DOUBLE
                    else (IMMEDIATE, SIGNED)
                    if sc is SIGNED
                    else OBJECT
                    for sc in convention.parameters
                )
                vop.info = replace(
                    vops.LOCAL_CALL,
                    args=(OBJECT, *restrictions),
                    arg_escapes=(
                        False,
                        *(sc is DESCRIPTOR for sc in convention.parameters),
                    ),
                    results=(
                        (DOUBLE,)
                        if convention.returned is DOUBLE
                        else (IMMEDIATE, SIGNED),
                    ),
                )
                vop.codegen_info = entry
                progress = changed = True
            if not progress:
                break
            pending = remaining
        return changed

    def fresh_box_result(self, component, vop):
        """Transfer fresh object ownership only across proven factory calls."""
        if vop.info is not vops.FULL_CALL:
            return None
        site = vop.codegen_info
        target = site.target
        if (
            not site.proven
            or site.keywords
            or target not in self.owned
            or target is component.lambda_
            or target.call_order((), len(vop.args) - 1)
            != tuple(range(len(target.vars)))
        ):
            return None
        layout = vop.results[0].tn.primitive_type.boxed_layout
        if layout is None:
            return None
        sources = tuple(ref.tn.type or UNKNOWN for ref in vop.args[1:])
        general, variants = self.contexts(target)
        contexts = {None: general, **{context.id: context for context in variants}}
        for selected, _guards in _entry_steps(self, target, sources):
            context = contexts[None if selected is None else selected.id]
            if primitive_type_of(context.return_type).boxed_layout is not layout:
                return None
            key = target, context.id, "object"
            if key not in self.fresh_box_summaries:
                # A pending entry is conservative, and only object conventions
                # are probed. No cloned node identities survive this scope.
                self.fresh_box_summaries[key] = None
                body = compile_component(target, context)
                fresh = fresh_box_returns(body, layout)
                native = self.native_plans.get(target)
                native_result = (
                    linked(native.returned, native, "return representation")
                    if native is not None
                    else None
                )
                if (
                    not fresh
                    and native is not None
                    and not native.dependencies
                    and native_result is not None
                    and native_result.primitive_type.boxed_layout is layout
                    and native_result.can_adapt_return(body, native)
                ):
                    # The actual object entry necessarily constructs its result
                    # from raw bits; no dependency fallback can return an alias.
                    fresh = True
                if fresh:
                    self.fresh_box_summaries[key] = layout
            if self.fresh_box_summaries[key] is not layout:
                return None
        return layout

    def rejected_fixnum_entry(self, function: Lambda, guard: FixnumGuard) -> str:
        if function not in self.rejected_fixnum_entries:
            general, _variants = self.contexts(function)
            self.rejected_fixnum_entries[function] = replace(
                general, rejected_fixnum_guard=guard
            )
        return _suffixed(self.bodies[function], ".fallback.fixnum")

    def emit(self) -> str:
        program = self.program
        functions = []
        variants_by_key: dict[tuple[Function, object], tuple[Lambda, EntryContext]] = {}
        contexts_by_key: dict[tuple[Function, object], tuple[Lambda, EntryContext]] = {}
        for lambda_ in program.lambdas:
            general, variants = self.contexts(lambda_)
            contexts_by_key[lambda_, None] = (lambda_, general)
            functions.append(self.emit_body(lambda_, general))
            for variant in variants:
                variants_by_key[lambda_, variant.id] = (lambda_, variant)
                contexts_by_key[lambda_, variant.id] = (lambda_, variant)
        emitted_variants = set()
        emitted_unpacked = set()
        emitted_numeric = set()
        emitted_rejected_fixnum = set()
        while True:
            for key, context in self.backend_contexts.items():
                variants_by_key[key] = (key[0], context)
            pending_variants = (
                self.specialized.requested & variants_by_key.keys()
            ) - emitted_variants
            pending_unpacked = (
                self.unpacked.requested & contexts_by_key.keys()
            ) - emitted_unpacked
            pending_numeric = self.numeric_entries.keys() - emitted_numeric
            pending_rejected = (
                self.rejected_fixnum_entries.keys() - emitted_rejected_fixnum
            )
            if not (
                pending_variants
                or pending_unpacked
                or pending_numeric
                or pending_rejected
            ):
                break
            for key in sorted(
                pending_variants, key=lambda item: (item[0].name, str(item[1]))
            ):
                emitted_variants.add(key)
                lambda_, variant = variants_by_key[key]
                functions.append(self.emit_body(lambda_, variant))
            for key in sorted(
                pending_unpacked, key=lambda item: (item[0].name, str(item[1]))
            ):
                emitted_unpacked.add(key)
                lambda_, context = contexts_by_key[key]
                functions.append(self.emit_body(lambda_, context, unpacked=True))
            for key in sorted(
                pending_numeric, key=lambda item: (item[0].name, str(item[1]))
            ):
                emitted_numeric.add(key)
                entry = self.numeric_entries[key]
                functions.append(self.emit_body(entry.function, entry.context))
            for function in sorted(pending_rejected, key=lambda item: item.name):
                emitted_rejected_fixnum.add(function)
                functions.append(
                    self.emit_body(function, self.rejected_fixnum_entries[function])
                )
        for lambda_, plan in self.native_plans.items():
            facts = self.facts[lambda_]
            context = EntryContext(
                "native", facts.types, facts.targets, (), facts.return_type, plan
            )
            functions.append(self.emit_body(lambda_, context))
        functions.extend(
            native_callbacks(self, layout) for layout in self.layouts.values()
        )
        owned_imports = [
            info for info in self.imports.values() if self.owns_import(info)
        ]
        functions.extend(self.import_initializer(info) for info in owned_imports)
        if program.bootstrap:
            functions.extend(
                self.missing_import_initializer(info, linkage="weak")
                for info in self.imports.values()
                if not self.owns_import(info)
            )
        attribute_lookup = self.attribute_lookup_adapter()
        if attribute_lookup is not None:
            functions.append(attribute_lookup)
        self.auxiliary.extend(
            f"{layout.schema} = internal global i8 0"
            for layout in self.layouts.values()
        )
        self.auxiliary.append(self.unit_lifecycle(owned_imports))
        self.auxiliary.extend(self.import_registration(owned_imports))
        if program.entry is not None:
            self.auxiliary.extend(self.executable_entry())
        self.auxiliary.extend(self.global_linkage())
        # Data first: the bodies emitted above registered their constants.
        globals_ = [
            f"{name} = private unnamed_addr constant "
            f'[{len(value) + 1} x i8] c"{"".join(map(_HEX_BYTE.__getitem__, value))}\\00"'
            for value, name in self.data.items()
        ]
        foreign_decls = self.foreign_declarations()
        body_sections = [
            section
            for section in (foreign_decls, *globals_, *self.auxiliary, *functions)
            if section
        ]
        body_text = "\n\n".join(body_sections)
        intrinsics_lines, helper_defs = _parsed_helpers(program.bootstrap)
        referenced = set(_MULE_SYMBOL_RE.findall(body_text))
        for llvm_sym in (
            "@llvm.assume",
            "@llvm.sadd.with.overflow.i64",
            "@llvm.ssub.with.overflow.i64",
            "@llvm.smul.with.overflow.i64",
        ):
            if llvm_sym in body_text:
                referenced.add(llvm_sym)
        kept_helpers: list[str] = []
        kept_symbols: set[str] = set()
        search_text = "\n".join(referenced)
        changed = True
        while changed:
            changed = False
            for symbol, chunk in helper_defs:
                if symbol not in kept_symbols and f"{symbol}(" in search_text:
                    kept_symbols.add(symbol)
                    kept_helpers.append(chunk)
                    search_text += "\n" + chunk
                    changed = True
        kept_decls = [
            line
            for symbol, line in (*_DECLARATION_LINES, *intrinsics_lines)
            if symbol in search_text
        ]
        return (
            "\n\n".join(
                [
                    "\n".join(kept_decls),
                    *kept_helpers,
                    *([body_text] if body_text else []),
                ]
            )
            + "\n"
        )

    def foreign_declarations(self) -> str:
        lines = []
        target = self.sequence_functions.get("get_attribute")
        if target is not None and target not in self.owned:
            lines.append("declare ptr @mule.attribute.lookup(ptr, ptr)")
        for name, (kind, lambda_) in sorted(self.foreign.items()):
            if kind == "fn":
                lines.append(f"declare ptr {_symbol(name)}(ptr, ptr)")
                continue
            parameters = ", ".join(
                ["ptr"]
                + (["ptr"] if self.closure_of(lambda_) else [])
                + ["ptr"] * len(lambda_.parameters)
            )
            lines.append(f"declare ptr {_symbol(name)}({parameters})")
        if not self.program.bootstrap:
            for info in self.imports.values():
                if not self.owns_import(info):
                    lines.append(f"declare ptr {self.import_symbol(info)}()")
        return "\n".join(lines)

    def import_registration(self, imports) -> list[str]:
        artifacts = [info for info in imports if not info.missing]
        if not artifacts:
            return []
        entry = _symbol(f"mule.unit:{self.program.unit}.register.imports")
        calls = []
        for info in artifacts:
            filename = "null" if info.filename is None else self.text(info.filename)
            locations = "null"
            if info.package_paths:
                locations = _symbol(f"mule.import.paths:{info.module_name}")
                self.auxiliary.append(
                    f"{locations} = private constant [{len(info.package_paths)} x ptr] ["
                    + ", ".join(f"ptr {self.text(path)}" for path in info.package_paths)
                    + "]"
                )
            calls.append(
                f"  call void @mule_register_compiled_module(ptr {self.text(info.module_name)}, "
                f"ptr {self.import_symbol(info)}, ptr {self.import_create_symbol(info)}, "
                f"ptr {self.import_execute_symbol(info)}, ptr {filename}, "
                f"ptr {locations}, i64 {len(info.package_paths)})"
            )
        return [
            (
                "@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] "
                f"[{{ i32, ptr, ptr }} {{ i32 65535, ptr {entry}, ptr null }}]"
            ),
            f"define internal void {entry}() {{\nentry:\n"
            + "\n".join(calls)
            + "\n  ret void\n}",
        ]

    def attribute_lookup_adapter(self) -> str | None:
        """Borrowed native arguments enter the owned compiled lookup protocol."""
        target = self.sequence_functions.get("get_attribute")
        if target not in self.owned:
            return None
        return (
            "define ptr @mule.attribute.lookup(ptr %receiver, ptr %name) {\n"
            "entry:\n"
            "  %frame = call i32 @mule_enter_frame()\n"
            "  %bad = icmp slt i32 %frame, 0\n"
            "  br i1 %bad, label %failed, label %entered\n"
            "entered:\n"
            "  %globals = call ptr @mule_protocol_globals()\n"
            "  %owned.receiver = call ptr @mule_retain(ptr %receiver)\n"
            "  %owned.name = call ptr @mule_retain(ptr %name)\n"
            f"  %result = call ptr {self.bodies[target]}(ptr %globals, ptr %owned.receiver, ptr %owned.name)\n"
            "  call void @mule_leave_frame()\n"
            "  ret ptr %result\n"
            "failed:\n"
            "  ret ptr null\n"
            "}"
        )

    def unit_lifecycle(self, owned_imports) -> str:
        """The unit's initializer, which links its protocol cells, and its
        finalizer, which releases its constants and the modules it owns."""
        linkage = "" if self.program.exports else "internal "
        # Protocol code runs under the protocol globals whoever calls it,
        # and the unit initializer runs after they exist. Only the unit with
        # the protocols' module bodies (the builtins unit) binds names there;
        # elsewhere protocol code (the format, argument and native class
        # protocols) only reads builtins, so its cells join no namespace.
        links = []
        if PROTOCOL_NAMESPACE in self.global_cells:
            target = "%protocol.globals"
            if not any(
                lambda_.kind is FunctionalKind.TOPLEVEL
                and runtime_namespace(lambda_.namespace) == PROTOCOL_NAMESPACE
                for lambda_ in self.program.lambdas
            ):
                if PROTOCOL_NAMESPACE in self.bound_namespaces:
                    raise AssertionError("protocol code binds globals outside its unit")
                target = "null"
            links.append("  %protocol.globals = call ptr @mule_protocol_globals()")
            links += self.link(PROTOCOL_NAMESPACE, target, "link.failed", 0)
        initialize = (
            f"define {linkage}ptr {unit_initializer(self.program.unit)}() {{\n"
            "entry:\n"
            + "".join(line + "\n" for line in links)
            + "  ret ptr inttoptr (i64 1 to ptr)\n"
            + ("link.failed:\n  ret ptr null\n" if links else "")
            + "}"
        )
        releases = (
            [
                (f"%import.{index}", self.import_cache(info))
                for index, info in enumerate(owned_imports)
                if not info.missing
            ]
            + [
                (f"%literal.root.{index}", constant.slot)
                for index, constant in reversed(
                    list(enumerate(self.tuple_constants.values()))
                )
            ]
            + [
                (f"%float.root.{index}", slot)
                for index, (slot, _) in enumerate(self.float_constants.values())
            ]
            + [
                (f"%text.root.{index}", slot)
                for index, (slot, _) in enumerate(self.text_constants.values())
            ]
            + [
                (f"%bytes.root.{index}", slot)
                for index, (slot, _) in enumerate(self.bytes_constants.values())
            ]
            + [
                (f"%int.root.{index}", slot)
                for index, (slot, _) in enumerate(self.bigint_constants.values())
            ]
        )
        finalize = (
            f"define {linkage}void {unit_finalizer(self.program.unit)}() {{\n"
            "entry:\n"
            + "".join(
                f"  {value} = load ptr, ptr {slot}\n"
                f"  call void @mule_release(ptr {value})\n"
                f"  store ptr null, ptr {slot}\n"
                for value, slot in releases
            )
            + "  ret void\n}"
        )
        return initialize + "\n\n" + finalize

    def executable_entry(self) -> list[str]:
        """``main`` and the protocol startup it runs: the units it depends on,
        the sequence and print protocols, then this unit, whose cells copy
        the builtins those protocols install."""
        program = self.program
        paths = "null"
        if program.search_paths:
            paths = "@mule.image.search.paths"
            self.auxiliary.append(
                f"{paths} = private constant [{len(program.search_paths)} x ptr] ["
                + ", ".join(f"ptr {self.text(path)}" for path in program.search_paths)
                + "]"
            )
        units = [*program.dependencies, program.unit]
        definitions = [
            f"declare ptr {unit_initializer(unit)}()\ndeclare void {unit_finalizer(unit)}()"
            for unit in program.dependencies
        ]
        startup = []
        for index, unit in enumerate(program.dependencies):
            startup += [
                f"  %unit.{index} = call ptr {unit_initializer(unit)}()",
                f"  %unit.bad.{index} = icmp eq ptr %unit.{index}, null",
                f"  br i1 %unit.bad.{index}, label %failure, label %unit.ready.{index}",
                f"unit.ready.{index}:",
            ]
        definitions.append(
            "define internal ptr @mule.initialize.protocols(ptr %globals, ptr %args) {\n"
            "entry:\n"
            + "\n".join(startup)
            + "\n"
            + f"  %sequence = call ptr {self.functions[linked(program.protocol_entry, program, 'protocol entry')]}(ptr %globals, ptr %args)\n"
            "  %failed = icmp eq ptr %sequence, null\n"
            "  call void @mule_release(ptr %sequence)\n"
            "  br i1 %failed, label %failure, label %print\n"
            "print:\n"
            + (
                f"  %format = call ptr {self.functions[linked(program.format_entry, program, 'format entry')]}(ptr %globals, ptr %args)\n"
                "  %format.failed = icmp eq ptr %format, null\n"
                "  call void @mule_release(ptr %format)\n"
                "  br i1 %format.failed, label %failure, label %format.ready\n"
                "format.ready:\n"
                if program.format_entry is not None
                else ""
            )
            + f"  %result = call ptr {self.functions[linked(program.print_entry, program, 'print entry')]}(ptr %globals, ptr %args)\n"
            "  %print.failed = icmp eq ptr %result, null\n"
            "  br i1 %print.failed, label %failure, label %unit\n"
            "unit:\n"
            "  call void @mule_set_attribute_lookup(ptr @mule.attribute.lookup)\n"
            f"  %own = call ptr {unit_initializer(program.unit)}()\n"
            "  %own.failed = icmp eq ptr %own, null\n"
            "  br i1 %own.failed, label %own.failure, label %ready\n"
            "ready:\n"
            "  ret ptr %result\n"
            "own.failure:\n"
            "  call void @mule_release(ptr %result)\n"
            "  br label %failure\n"
            "failure:\n"
            "  ret ptr null\n"
            "}"
        )
        if program.argument_constructor is not None:
            definitions.append(
                "define internal ptr @mule.arguments.construct(ptr %class, ptr %view) {\n"
                "entry:\n"
                "  %globals = call ptr @mule_protocol_globals()\n"
                "  %owned.class = call ptr @mule.fast.retain(ptr %class)\n"
                "  %owned.view = call ptr @mule.fast.retain(ptr %view)\n"
                f"  %result = call ptr {self.bodies[program.argument_constructor]}(ptr %globals, ptr %owned.class, ptr %owned.view)\n"
                "  ret ptr %result\n}"
            )
        run = (
            f"call i32 @mule_run_protocols(ptr {self.functions[linked(program.entry, program, 'entry')]}, "
            "ptr @mule.initialize.protocols)"
        )
        definitions.append(
            "define i32 @main(i32 %argc, ptr %argv) {\n"
            "entry:\n"
            "  call void @mule_set_process_arguments(i32 %argc, ptr %argv)\n"
            + f"  call void @mule_set_image_search_path(ptr {paths}, i64 {len(program.search_paths)})\n"
            + (
                "  call void @mule_set_argument_constructor(ptr @mule.arguments.construct)\n"
                if program.argument_constructor is not None
                else ""
            )
            + f"  %status = {run}\n"
            + "  call void @mule_run_finalizers()\n"
            + "".join(
                f"  call void {unit_finalizer(unit)}()\n" for unit in reversed(units)
            )
            + "  ret i32 %status\n"
            "}"
        )
        return definitions

    def emit_body(self, lambda_: Lambda, context, *, unpacked: bool = False) -> str:
        with (
            phase(
                "LLVM body %s context %s",
                lambda_.name,
                context.id,
                level=logging.DEBUG,
            ),
            specialized_ir1_context(lambda_, context),
        ):
            component = compile_component(
                lambda_, context, select_calls=self.select_numeric_calls
            )
            mark_private_boxes(
                component,
                fresh_result=lambda vop: self.fresh_box_result(component, vop),
            )
            with phase("VOP generation %s", lambda_.name, level=logging.DEBUG):
                body = FunctionGenerator(self, component, unpacked=unpacked).emit()
        logger.debug(
            "IR2 body %s context %s: %d blocks, %d TNs",
            lambda_.name,
            context.id,
            len(component.blocks),
            len(component.tns),
        )
        return body

    def unit_startup(self, info) -> list[str]:
        """A module unit initializes itself when its module is first imported,
        before any of its functions can exist, and releases its state at exit."""
        own = self.program.module_import
        if own is None or info.module_name != own.module_name:
            return []
        unit = self.program.unit
        return [
            f"  %unit = call ptr {unit_initializer(unit)}()",
            "  %unit.bad = icmp eq ptr %unit, null",
            "  br i1 %unit.bad, label %unit.failed, label %unit.ready",
            "unit.failed:",
            "  ret ptr null",
            "unit.ready:",
            f"  call void @mule_register_finalizer(ptr {unit_finalizer(unit)})",
        ]

    def import_cache(self, info) -> str:
        return _symbol(f"mule.import.cache:{info.module_name}")

    def import_create_symbol(self, info) -> str:
        return _symbol(f"mule.import.create:{info.module_name}")

    def import_execute_symbol(self, info) -> str:
        return _symbol(f"mule.import.execute:{info.module_name}")

    def missing_import_initializer(self, info, linkage: str = "internal") -> str:
        symbol = self.import_symbol(info)
        return "\n".join(
            [
                f"define {linkage} ptr {symbol}() {{",
                "entry:",
                f"  %module = call ptr @mule_import_missing(ptr {self.text(info.module_name)})",
                "  ret ptr %module",
                "}",
            ]
        )

    def import_initializer(self, info):
        if info.missing:
            # Without compiled source, a module can still be one the program
            # installed in sys.modules; otherwise this raises ModuleNotFoundError.
            return self.missing_import_initializer(info)
        symbol = self.import_symbol(info)
        # Every unit importing a native module generates the same initializer;
        # the linker keeps one, so the module stays a singleton. A compiled
        # module's initializer is defined only by the unit owning its body.
        linkage = "linkonce_odr " if info.native else ""
        cache = self.import_cache(info)
        self.auxiliary.append(f"{cache} = {linkage or 'internal '}global ptr null")
        legacy = [
            f"define {linkage}ptr {symbol}() {{",
            "entry:",
            f"  %cached = load ptr, ptr {cache}",
            "  %ready = icmp ne ptr %cached, null",
            "  br i1 %ready, label %existing, label %initialize",
            "existing:",
            f"  %owned = call ptr @mule_import_missing(ptr {self.text(info.module_name)})",
            "  ret ptr %owned",
            "initialize:",
            f"  %globals = call ptr {self.import_create_symbol(info)}()",
            "  %bad.globals = icmp eq ptr %globals, null",
            "  br i1 %bad.globals, label %failed, label %created",
            "created:",
            f"  call void @mule_module_loaded(ptr {self.text(info.module_name)}, ptr %globals)",
            f"  %result = call ptr {self.import_execute_symbol(info)}(ptr %globals)",
            "  call void @mule_release(ptr %globals)",
            "  %bad.result = icmp eq ptr %result, null",
            "  call void @mule_release(ptr %result)",
            "  br i1 %bad.result, label %failed, label %finished",
            "finished:",
            f"  %module = call ptr @mule_import_missing(ptr {self.text(info.module_name)})",
            "  ret ptr %module",
            "failed:",
            f"  call void @mule_module_failed(ptr {self.text(info.module_name)})",
            "  ret ptr null",
            "}",
        ]
        lines = [
            f"define {linkage}ptr {self.import_create_symbol(info)}() {{",
            "entry:",
            *self.unit_startup(info),
            # The builtins module is the namespace builtin lookups consult.
            "  %globals = call ptr @mule_builtins_module()"
            if info.module_name == "builtins"
            else "  %globals = call ptr @mule_namespace()",
        ]
        if info.native:
            self.shared_namespaces.add(info.module_name)
        fields = [("__name__", info.module_name), ("__package__", info.package_name)]
        if info.filename is not None:
            fields.append(("__file__", info.filename))
        for field_name, value in fields:
            literal = self.literal_data(value.encode())
            label = field_name.strip("_")
            store = f"call i32 @mule_namespace_bind(ptr %globals, ptr {self.text(field_name)}, ptr %{label})"
            lines.extend(
                [
                    f"  %{label} = call ptr @mule_literal_text(ptr {literal}, i64 {len(value.encode())})",
                    f"  %status.{label} = {store}",
                    f"  call void @mule_release(ptr %{label})",
                    f"  %bad.{label} = icmp slt i32 %status.{label}, 0",
                    f"  br i1 %bad.{label}, label %failed, label %after.{label}",
                    f"after.{label}:",
                ]
            )
        lines.append("  %metadata.none = call ptr @mule_none()")
        absent_fields = ["__spec__", "__loader__"]
        if info.namespace_paths:
            absent_fields.append("__file__")
        for field_name in absent_fields:
            store = f"call i32 @mule_namespace_bind(ptr %globals, ptr {self.text(field_name)}, ptr %metadata.none)"
            label = field_name.strip("_")
            lines.extend(
                [
                    f"  %status.{label} = {store}",
                    f"  %bad.{label} = icmp slt i32 %status.{label}, 0",
                    f"  br i1 %bad.{label}, label %failed, label %after.{label}",
                    f"after.{label}:",
                ]
            )
        if info.package_paths:
            count = len(info.package_paths)
            store = f"call i32 @mule_namespace_bind(ptr %globals, ptr {self.text('__path__')}, ptr %path)"
            lines.append(f"  %directory.slots = alloca [{count} x ptr]")
            for index, path in enumerate(info.package_paths):
                directory = path.encode()
                literal = self.literal_data(directory)
                lines.extend(
                    [
                        f"  %directory.{index} = call ptr @mule_literal_text(ptr {literal}, i64 {len(directory)})",
                        f"  %directory.slot.{index} = getelementptr [{count} x ptr], ptr %directory.slots, i32 0, i32 {index}",
                        f"  store ptr %directory.{index}, ptr %directory.slot.{index}",
                    ]
                )
            lines.extend(
                [
                    f"  %path = call ptr @mule_sequence_list(i64 {count}, ptr %directory.slots)",
                    *(
                        f"  call void @mule_release(ptr %directory.{index})"
                        for index in range(count)
                    ),
                    f"  %status.path = {store}",
                    "  call void @mule_release(ptr %path)",
                    "  %bad.path = icmp slt i32 %status.path, 0",
                    "  br i1 %bad.path, label %failed, label %after.path",
                    "after.path:",
                ]
            )
        # Creation has no body effects or sys.modules registration. Importlib
        # can set the spec and insert the module before executing native code.
        lines.extend(
            [
                "  ret ptr %globals",
                "failed:",
                "  call void @mule_release(ptr %globals)",
                "  ret ptr null",
                "}",
            ]
        )
        create = "\n".join(lines)
        lines = [
            f"define {linkage}ptr {self.import_execute_symbol(info)}(ptr %globals) {{",
            "entry:",
            f"  %previous = load ptr, ptr {cache}",
            "  %owned.globals = call ptr @mule_retain(ptr %globals)",
            f"  store ptr %owned.globals, ptr {cache}",
            "  call void @mule_release(ptr %previous)",
        ]
        lines += self.link(info.module_name, "%globals", "unregistered.failure", 0)
        if info.native:
            lines.append(f"  %result = call ptr @{info.initializer}(ptr %globals)")
        else:
            lines.extend(
                [
                    "  %frame = call i32 @mule_enter_frame()",
                    "  %bad.frame = icmp slt i32 %frame, 0",
                    "  br i1 %bad.frame, label %failed, label %execute",
                    "execute:",
                    f"  %result = call ptr {self.bodies_by_name(info.initializer)}(ptr %globals)",
                    "  call void @mule_leave_frame()",
                ]
            )
        lines.extend(
            [
                "  %bad.result = icmp eq ptr %result, null",
                "  call void @mule_release(ptr %result)",
                "  br i1 %bad.result, label %failed, label %success",
                "success:",
                "  %module = call ptr @mule_retain(ptr %globals)",
                "  ret ptr %module",
                "failed:",
                "  br label %unregistered.failure",
                "unregistered.failure:",
                f"  store ptr null, ptr {cache}",
                "  call void @mule_release(ptr %globals)",
                "  ret ptr null",
                "}",
            ]
        )
        return "\n\n".join(("\n".join(legacy), create, "\n".join(lines)))


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
    if tn.leaf is not None:
        return False
    if not any(ref.tn is tn for ref in vop.args):
        return False
    if vop.info.name == "unpack-ref" and vop.args[0].tn is tn:
        return not vop.codegen_info.borrow_parent
    return not (vop.info.name == "sequence-available" and vop.args[0].tn is tn)


def _value_tns_live_in(component, candidates) -> dict:
    """Backward liveness of the cross-block temporaries in ``candidates``:
    for each IR2 block, those read on some path from its start before being
    written. Error edges count as successors."""
    uses, defs, successors = {}, {}, {}
    for block in component.blocks:
        used, defined = set(), set()
        for vop in block.vops:
            for ref in (*vop.args, *vop.results):
                tn = ref.tn
                if tn not in candidates:
                    continue
                if tn not in defined and _vop_reads_tn(vop, tn):
                    used.add(tn)
                if any(res.tn is tn for res in vop.results):
                    defined.add(tn)
        uses[block], defs[block] = used, defined
        following = [
            component.ir2_blocks[succ]
            for succ in block.block.succ
            if succ in component.ir2_blocks
        ]
        target = block_exception_target(block.block)
        if target is not None and target in component.ir2_blocks:
            following.append(component.ir2_blocks[target])
        successors[block] = following
    live = {block: set(uses[block]) for block in component.blocks}
    changed = True
    while changed:
        changed = False
        for block in reversed(component.blocks):
            out = set()
            for succ in successors[block]:
                out |= live[succ]
            incoming = uses[block] | (out - defs[block])
            if incoming != live[block]:
                live[block] = incoming
                changed = True
    return live


def _pack_slot_tns(component, candidates, fixed, prefix: str) -> list[list]:
    """Assign stack slots to ``fixed`` (one each) and ``candidates`` (packing
    non-overlapping TNs of the same storage class and cleanup policy, following
    SBCL ``life.lisp``/``pack.lisp``). Returns the list of TN groups per slot."""
    import heapq

    groups: list[list] = []
    for tn in fixed:
        tn.slot = f"{prefix}.{len(groups)}"
        groups.append([tn])
    if not candidates:
        return groups
    candidate_set = set(candidates)
    owned_set = {tn for tn in candidates if _needs_cleanup_release(tn)}
    blocks = component.blocks
    ir2_blocks = component.ir2_blocks
    block_start = {}
    block_end = {}
    normal_succs = {}
    handlers = {}
    live_use = {}
    live_kill = {}
    occ_gen = {}
    occ_kill = {}
    occ_mid = {}
    first_step = {}
    last_step = {}
    step = 0
    for block in blocks:
        start_step = step
        used, defined = set(), set()
        gen, kill, mid = set(), set(), set()
        for vop in block.vops:
            touched = {
                ref.tn for ref in (*vop.args, *vop.results) if ref.tn in candidate_set
            }
            for tn in touched:
                if tn not in first_step:
                    first_step[tn] = step
                last_step[tn] = step
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
        live_use[block] = used
        live_kill[block] = defined
        occ_gen[block] = gen
        occ_kill[block] = kill
        occ_mid[block] = mid
        normal_succs[block] = [
            ir2_blocks[succ] for succ in block.block.succ if succ in ir2_blocks
        ]
        handlers[block] = ir2_blocks.get(block_exception_target(block.block))
    live_in = {block: set(live_use[block]) for block in blocks}
    live_out = {block: set() for block in blocks}
    changed = True
    while changed:
        changed = False
        for block in reversed(blocks):
            out = set()
            for succ in normal_succs[block]:
                out |= live_in[succ]
            handler = handlers[block]
            if handler is not None:
                out |= live_in[handler]
            incoming = live_use[block] | (out - live_kill[block])
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
    for block in blocks:
        b_start = block_start[block]
        b_end = block_end[block]
        for tn in live_in[block] | occ_in[block]:
            if tn not in first_step or b_start < first_step[tn]:
                first_step[tn] = b_start
            if tn not in last_step or b_start > last_step[tn]:
                last_step[tn] = b_start
        for tn in live_out[block] | occ_out[block]:
            if tn not in first_step or b_end < first_step[tn]:
                first_step[tn] = b_end
            if tn not in last_step or b_end > last_step[tn]:
                last_step[tn] = b_end
    by_kind: dict[tuple, list] = {}
    for tn in candidates:
        if tn not in first_step:
            continue
        key = (tn.sc, tn in owned_set, tn.private_box)
        by_kind.setdefault(key, []).append(tn)
    for kind_tns in by_kind.values():
        kind_tns.sort(key=lambda tn: (first_step[tn], last_step[tn]))
        active_heap: list[tuple[int, int]] = []
        free_groups: list[int] = []
        for tn in kind_tns:
            start = first_step[tn]
            end = last_step[tn]
            while active_heap and active_heap[0][0] < start:
                _, freed_idx = heapq.heappop(active_heap)
                free_groups.append(freed_idx)
            if free_groups:
                group_idx = free_groups.pop()
                groups[group_idx].append(tn)
            else:
                group_idx = len(groups)
                groups.append([tn])
            tn.slot = f"{prefix}.{group_idx}"
            heapq.heappush(active_heap, (end, group_idx))
    for tn in candidates:
        if tn not in first_step:
            group_idx = len(groups)
            groups.append([tn])
            tn.slot = f"{prefix}.{group_idx}"
    return groups


class _HelperBuilder:
    """Line and SSA temporary emitter for shared module-level call helpers."""

    def __init__(self, module=None, *, failure_result="ptr null") -> None:
        self.lines: list[str] = []
        self.serial = 0
        # The module generator, for guards that name module-level data.
        self.module = module
        self.failure_result = failure_result

    def line(self, text: str) -> None:
        self.lines.append(text if text.startswith("  ") else "  " + text)

    def operation(self, text: str) -> str:
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
        self.line(f"br i1 {condition}, label %{rejected}, label %{accepted}")
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
        if fact.literal is not None:
            return self.visit_ValueType(fact)
        matched = self.gen.operation(f"call ptr @mule_text_storage(ptr {self.value})")
        return self.gen.operation(f"icmp eq ptr {matched}, @mule.true")

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

    def visit_ListType(self, fact: ListType) -> str:
        if fact.element != UNKNOWN or fact.may_be_unbound:
            return self.visit_ValueType(fact)
        matched = self.gen.operation(f"call i32 @mule_exact_list(ptr {self.value})")
        return self.gen.operation(f"icmp ne i32 {matched}, 0")

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
            matched = self.gen.operation(
                f"call i32 @mule_tuple_shape_matches(ptr {self.value}, i64 {arity})"
            )
            valid = self.gen.operation(f"icmp ne i32 {matched}, 0")
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
                f"call ptr @mule_tuple_borrow_item(ptr {self.value}, i64 {index})"
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
        layout = self.gen.operation(f"call i64 @mule.attr.layout(ptr {self.value})")
        expected = self.gen.operation(
            f"load i64, ptr {self.gen.module.class_layout_slot(fact.identity)}"
        )
        known = self.gen.operation(f"icmp ne i64 {expected}, 0")
        same = self.gen.operation(f"icmp eq i64 {layout}, {expected}")
        return self.gen.operation(f"and i1 {known}, {same}")

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
            if fact.minimum > FIXNUM_MIN:
                lower = operation(f"icmp sge i64 {decoded}, {fact.minimum}")
                valid = operation(f"and i1 {valid}, {lower}")
            if fact.maximum < FIXNUM_MAX:
                upper = operation(f"icmp sle i64 {decoded}, {fact.maximum}")
                valid = operation(f"and i1 {valid}, {upper}")
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


class FunctionGenerator:
    """Emit one LLVM function for one IR2 component (one body)."""

    def __init__(self, module: ModuleGenerator, component, *, unpacked: bool = False):
        self.module = module
        self._literal_emitter = vops.LiteralEmitter(module)
        self._borrowed_literal_emitter = vops.BorrowedLiteralEmitter(module)
        self.component = component
        self.lambda_ = component.lambda_
        self.context = component.context
        self.plan = self.context.plan
        self.raw = self.plan is not None
        self.convention = self.context.convention
        self.unpacked = unpacked
        if unpacked:
            info = _return_tuple_info(self.lambda_)
            self.return_uses = (
                info[1]
                if info is not None
                else module.return_tuple_uses.get(self.lambda_, frozenset())
            )
        else:
            self.return_uses = frozenset()
        self.environment = component.environment
        self.immortal_values: set[str] = set()
        self.borrowed_values: set[str] = set()
        self.heap_values: set[str] = set()
        self.private_values: dict[str, BoxLayout] = {}
        self.lines: list[str] = []
        self.serial = 0
        self.exception_target: str | None = None
        if self.raw:
            self.body = self.plan.entry
        elif self.convention is not None:
            self.body = module.numeric_entries[self.lambda_, self.context.id].symbol
        elif self.unpacked:
            self.body = module.unpacked[self.lambda_, self.context.id]
        elif self.context.rejected_fixnum_guard is not None:
            self.body = _suffixed(module.bodies[self.lambda_], ".fallback.fixnum")
        elif self.context.id is not None:
            self.body = module.specialized[self.lambda_, self.context.id]
        else:
            self.body = module.bodies[self.lambda_]
        if self.unpacked:
            for block in component.blocks:
                for vop in block.vops:
                    if (
                        vop.node is not None
                        and vop.node in self.return_uses
                        and vop.results
                    ):
                        vop.results[0].tn.type = NONE_TYPE
        self._find_unpacked_calls(component)
        self.local_slots: dict[str, str] = {}
        self.local_tns_by_name: dict[str, TN] = {}
        self.value_tns = []
        self.local_tns = []
        self.range_tns = []
        # Temporaries confined to one block are LLVM values rather than
        # slots: ``values`` holds each one's definition, ``live`` the owned
        # ones not yet consumed, which a failure edge must release, and
        # ``expiring`` the vop after which an unconsumed one is released.
        self.registers: set = set()
        self.values: dict = {}
        self.live: dict = {}
        self.expiring: dict = {}
        positions = {
            vop: (block, index)
            for block in component.blocks
            for index, vop in enumerate(block.vops)
        }
        reads = component.reads
        writes = component.writes
        for tn in component.tns:
            if tn.primitive_type is not None and tn.sc is RANGE:
                self.range_tns.append(tn)
                tn.slot = f"%range.{len(self.range_tns) - 1}"
            elif tn.leaf is not None and tn.leaf.expression_temporary:
                # A variable's slot is loaded and stored by name, so an
                # expression temporary stays in a slot, released when an
                # exception transfers out of the expression.
                if tn not in self.unpacked_temp_tns:
                    self.value_tns.append(tn)
            elif tn.leaf is None and not (
                tn.name is not None and tn.name.startswith("$")
            ):
                # Compiler temporaries. The leafless "$cell:"/"$copy:" TNs
                # holding captures are locals, below.
                last = _block_local_last_vop(tn, positions, reads, writes)
                if last is not None:
                    tn.slot = None
                    self.registers.add(tn)
                    self.expiring.setdefault(last, []).append(tn)
                else:
                    self.value_tns.append(tn)
            else:
                self.local_tns.append(tn)
        packable_locals = [
            tn
            for tn in self.local_tns
            if tn.leaf is not None
            and tn.leaf.hidden
            and not tn.leaf.expression_temporary
            and tn.leaf not in self.environment.cells
            and tn.leaf not in self.environment.copies
        ]
        packable_set = set(packable_locals)
        fixed_locals = [tn for tn in self.local_tns if tn not in packable_set]
        self.local_slot_groups = _pack_slot_tns(
            component, packable_locals, fixed_locals, "%local"
        )
        self.value_slot_groups = _pack_slot_tns(component, self.value_tns, (), "%value")
        self.unwind_slot_groups = [
            *self.local_slot_groups[len(fixed_locals) :],
            *self.value_slot_groups,
        ]
        for tn in self.local_tns:
            if tn.leaf is not None:
                self.local_slots[tn.leaf.name] = tn.slot
                self.local_tns_by_name[tn.leaf.name] = tn
        self.landings: dict[tuple[str, tuple[str, ...], bool, bool], str] = {}
        # Each landing's predecessor blocks and the source line failing there.
        self.landing_sites: dict[str, dict[str, tuple[int, str]]] = {}
        self.vop = None
        self.checking_element_contract = False
        self.unpacked_remaining_slots: tuple[int, ...] = ()
        self.entry_end = 0
        self.uses_out = False
        self.uses_reuse = False
        self.uses_float_scratch = False
        self.uses_numeric_error = False
        self.max_argv = 0
        self.owner_slots: dict[object, str] = {}
        if self.raw:
            self.owner_slots = {
                node: f"%owner.{index}" for index, node in enumerate(self.plan.owners)
            }
            self.dependencies = {
                var: f"%class.{index}"
                for index, var in enumerate(self.plan.dependencies)
            }

    def _find_unpacked_calls(self, component) -> None:
        self.unpacked_calls: dict[object, int] = {}
        self.unpacked_call_assertions: dict[object, object] = {}
        self.unpacked_unpacks: set[object] = set()
        self.unpacked_slot_unpacks: dict[object, int] = {}
        self.unpacked_sequence_available: dict[object, int] = {}
        self.unpacked_moves: set[object] = set()
        self.unpacked_refs: dict[object, int] = {}
        self.unpacked_immortal_refs: set[object] = set()
        self.unpacked_dead_refs: set[object] = set()
        self.unpacked_discard_loads: set[object] = set()
        self.unpacked_temp_tns: set[object] = set()
        self.max_unpack_out = 0
        if self.raw:
            return
        arities = self.module.return_tuple_arities
        reads = component.reads
        writes = component.writes
        for block in component.blocks:
            vops_list = block.vops
            n_vops = len(vops_list)
            idx = 0
            while idx < n_vops:
                if idx + 2 >= n_vops:
                    break
                count_vop = vops_list[idx]
                unpack_vop = vops_list[idx + 1]
                move_vop = vops_list[idx + 2]
                if (
                    count_vop.info is not vops.CONSTANT
                    or unpack_vop.info.name != "structural-unpack"
                    or move_vop.info is not vops.MOVE
                ):
                    idx += 1
                    continue
                arity = unpack_vop.codegen_info.arity
                if arity is None or not (1 <= arity <= 8):
                    idx += 1
                    continue
                call_vop = None
                assertion_vop = None
                is_partition_call = False
                call_index = idx - 1
                if (
                    idx >= 2
                    and vops_list[idx - 1].info is vops.TYPE_CHECK
                    and vops_list[idx - 2].info is vops.FULL_CALL
                ):
                    assertion_vop = vops_list[idx - 1]
                    call_index = idx - 2
                if call_index >= 0 and vops_list[call_index].info is vops.FULL_CALL:
                    candidate = vops_list[call_index]
                    site = candidate.codegen_info
                    target = site.target
                    call_arity = arities.get(target) if target is not None else None
                    delivered = candidate.results[0].tn
                    if assertion_vop is not None:
                        if (
                            assertion_vop.args[0].tn is not delivered
                            or len(reads.get(delivered, ())) != 1
                            or not vops.checks_unpacked_return(
                                assertion_vop.codegen_info.contract, arity
                            )
                        ):
                            idx += 1
                            continue
                        delivered = assertion_vop.results[0].tn
                    if (
                        call_arity == arity
                        and target.call_order(site.keywords, len(candidate.args) - 1)
                        is not None
                        and unpack_vop.args[0].tn is delivered
                    ):
                        call_vop = candidate
                elif (
                    idx >= 1
                    and vops_list[idx - 1].info.name == "call-resolved"
                    and vops_list[idx - 1].codegen_info.exact_text_partition
                    and arity == 3
                    and unpack_vop.args[0].tn is vops_list[idx - 1].results[0].tn
                ):
                    call_vop = vops_list[idx - 1]
                    is_partition_call = True
                val_tn = unpack_vop.args[0].tn
                count_tn = count_vop.results[0].tn
                unpack_res_tn = unpack_vop.results[0].tn
                temp_tn = move_vop.results[0].tn
                if (
                    unpack_vop.args[1].tn is not count_tn
                    or move_vop.args[0].tn is not unpack_res_tn
                    or len(writes.get(val_tn, ())) != 1
                    or len(reads.get(val_tn, ())) != 1
                    or len(writes.get(count_tn, ())) != 1
                    or len(reads.get(count_tn, ())) != 1
                    or len(writes.get(unpack_res_tn, ())) != 1
                    or len(reads.get(unpack_res_tn, ())) != 1
                    or temp_tn.leaf is None
                    or not temp_tn.leaf.expression_temporary
                    or temp_tn in self.unpacked_temp_tns
                    or len(writes.get(temp_tn, ())) != 1
                    or len(reads.get(temp_tn, ())) != arity + 1
                ):
                    idx += 1
                    continue
                cursor = idx + 3
                ref_vops = []
                matched = True
                for slot_idx in range(arity):
                    if cursor + 2 >= n_vops:
                        matched = False
                        break
                    pos_vop = vops_list[cursor]
                    ref_vop = vops_list[cursor + 1]
                    if (
                        pos_vop.info is not vops.CONSTANT
                        or ref_vop.info.name != "unpack-ref"
                        or ref_vop.codegen_info.index != slot_idx
                        or not ref_vop.codegen_info.borrow_parent
                        or ref_vop.args[0].tn is not temp_tn
                        or ref_vop.args[1].tn is not pos_vop.results[0].tn
                        or len(writes.get(pos_vop.results[0].tn, ())) != 1
                        or len(reads.get(pos_vop.results[0].tn, ())) != 1
                    ):
                        matched = False
                        break
                    ref_res_tn = ref_vop.results[0].tn
                    if (
                        len(writes.get(ref_res_tn, ())) != 1
                        or len(reads.get(ref_res_tn, ())) != 1
                    ):
                        matched = False
                        break
                    cursor += 2
                    next_vop = vops_list[cursor]
                    has_type_check = False
                    if next_vop.info is vops.TYPE_CHECK:
                        has_type_check = True
                        check = next_vop.codegen_info
                        actual = next_vop.args[0].tn.type or UNKNOWN
                        if next_vop.args[0].tn is not ref_res_tn or (
                            check.safety
                            and not actual.fits(check.contract.checked_type)
                        ):
                            matched = False
                            break
                        ref_res_tn = next_vop.results[0].tn
                        if (
                            len(writes.get(ref_res_tn, ())) != 1
                            or len(reads.get(ref_res_tn, ())) != 1
                            or cursor + 1 >= n_vops
                        ):
                            matched = False
                            break
                        cursor += 1
                    store_vop = vops_list[cursor]
                    if (
                        store_vop.info is not vops.MOVE
                        or store_vop.args[0].tn is not ref_res_tn
                        or store_vop.results[0].tn is temp_tn
                    ):
                        matched = False
                        break
                    ref_vops.append((slot_idx, ref_vop, store_vop, has_type_check))
                    cursor += 1
                if not matched or cursor + 1 >= n_vops:
                    idx += 1
                    continue
                discard_load_vop = vops_list[cursor]
                discard_vop = vops_list[cursor + 1]
                if (
                    discard_load_vop.info is not vops.LOAD_LOCAL
                    or discard_load_vop.args[0].tn is not temp_tn
                    or not discard_load_vop.codegen_info.move
                    or discard_vop.info is not vops.DISCARD
                ):
                    idx += 1
                    continue
                discard_tn = discard_load_vop.results[0].tn
                if (
                    discard_vop.args[0].tn is not discard_tn
                    or len(writes.get(discard_tn, ())) != 1
                    or len(reads.get(discard_tn, ())) != 1
                ):
                    idx += 1
                    continue
                sep_is_immortal = (
                    call_vop is not None
                    and is_partition_call
                    and (call_vop.args[2].tn.type or UNKNOWN).is_immortal
                )
                if call_vop is not None:
                    self.unpacked_calls[call_vop] = arity
                    self.unpacked_unpacks.add(unpack_vop)
                    val_tn.type = NONE_TYPE
                    if assertion_vop is not None:
                        self.unpacked_call_assertions[call_vop] = assertion_vop
                        assertion_vop.args[0].tn.type = NONE_TYPE
                elif (
                    idx == 2
                    and vops_list[0].info is vops.CONSTANT
                    and vops_list[1].info.name == "protocol-next_item"
                    and vops_list[1].args[0].tn is vops_list[0].results[0].tn
                    and val_tn is vops_list[1].results[0].tn
                    and len(block.block.pred) == 1
                    and (pred_ir2 := component.ir2_blocks.get(block.block.pred[0]))
                    is not None
                    and len(pred_ir2.vops) >= 2
                    and pred_ir2.vops[-2].info.name == "sequence-available"
                    and pred_ir2.vops[-1].info is vops.BRANCH
                    and pred_ir2.vops[-1].codegen_info.consequent == block.label
                ):
                    self.unpacked_slot_unpacks[unpack_vop] = arity
                    self.unpacked_sequence_available[pred_ir2.vops[-2]] = arity
                elif (
                    idx >= 1
                    and vops_list[idx - 1].info.name == "call-resolved"
                    and val_tn is vops_list[idx - 1].results[0].tn
                ):
                    self.unpacked_slot_unpacks[unpack_vop] = arity
                else:
                    idx += 1
                    continue
                self.unpacked_moves.add(move_vop)
                for slot_idx, ref_vop, store_vop, has_type_check in ref_vops:
                    self.unpacked_refs[ref_vop] = (slot_idx, arity)
                    if sep_is_immortal and slot_idx == 1:
                        self.unpacked_immortal_refs.add(ref_vop)
                    target_tn = store_vop.results[0].tn
                    if (
                        not has_type_check
                        and (
                            ref_vop.node not in self.context.element_contracts
                            or not self.lambda_.policy.safety
                        )
                        and not reads.get(target_tn)
                        and len(writes.get(target_tn, ())) == 1
                        and target_tn.leaf is not None
                        and not target_tn.leaf.indirect
                        and (
                            self.environment is None
                            or (
                                target_tn.leaf not in self.environment.cells
                                and target_tn.leaf not in self.environment.copies
                            )
                        )
                    ):
                        self.unpacked_dead_refs.add(ref_vop)
                        self.unpacked_moves.add(store_vop)
                        self.unpacked_temp_tns.add(target_tn)
                        target_tn.type = NONE_TYPE
                self.unpacked_discard_loads.add(discard_load_vop)
                self.unpacked_temp_tns.add(temp_tn)
                self.max_unpack_out = max(self.max_unpack_out, arity)
                unpack_res_tn.type = NONE_TYPE
                temp_tn.type = NONE_TYPE
                discard_tn.type = NONE_TYPE
                idx = cursor + 2

    def is_unpacked_return_vop(self, vop) -> bool:
        return self.unpacked and vop.node is not None and vop.node in self.return_uses

    def unpacked_return_result(self, ref, values) -> None:
        for index, value in enumerate(values):
            slot = self.operation(f"getelementptr ptr, ptr %tuple.out, i64 {index}")
            self.line(f"store ptr {value}, ptr {slot}")
        self.immortal_values.add("@mule.none")
        self.publish(ref, "@mule.none")

    def unpacked_call_arity(self, vop) -> int | None:
        return self.unpacked_calls.get(vop)

    def unpacked_call_contract(self, vop):
        assertion = self.unpacked_call_assertions.get(vop)
        if assertion is None or not assertion.codegen_info.safety:
            return None
        return assertion.codegen_info.contract

    def is_unpacked_result_check(self, vop):
        return vop in self.unpacked_call_assertions.values()

    def is_unpacked_unpack(self, vop) -> bool:
        return vop in self.unpacked_unpacks

    def unpacked_slot_arity(self, vop) -> int | None:
        return self.unpacked_slot_unpacks.get(vop)

    def unpacked_sequence_available_arity(self, vop) -> int | None:
        return self.unpacked_sequence_available.get(vop)

    def is_unpacked_move(self, vop) -> bool:
        return vop in self.unpacked_moves

    def unpacked_ref_slot(self, vop) -> int | None:
        info = self.unpacked_refs.get(vop)
        return None if info is None else info[0]

    def unpacked_ref_remaining(self, vop) -> tuple[int, ...]:
        info = self.unpacked_refs.get(vop)
        if info is None:
            return ()
        slot_idx, arity = info
        return tuple(range(slot_idx + 1, arity))

    def is_unpacked_immortal_ref(self, vop) -> bool:
        return vop in self.unpacked_immortal_refs

    def is_unpacked_dead_ref(self, vop) -> bool:
        return vop in self.unpacked_dead_refs

    def is_unpacked_discard_load(self, vop) -> bool:
        return vop in self.unpacked_discard_loads

    # Text helpers

    def line(self, text: str) -> None:
        self.lines.append("  " + text)

    def fresh(self, prefix: str = "t") -> str:
        self.serial += 1
        return f"%{prefix}.{self.serial}"

    def operation(self, text: str) -> str:
        value = self.fresh()
        self.line(f"{value} = {text}")
        return value

    def failure_target(
        self,
        base: str | None = None,
        failed_value=None,
        *,
        record_traceback: bool = True,
        explicit_raise: bool = False,
    ) -> str:
        """The label a failure edge branches to: ``base`` (the handler's unwind
        block or ``error`` by default) after releasing the owned temporaries
        live here. ``failed_value`` is the NULL result being checked."""
        if base is None:
            base = (
                "error"
                if self.exception_target is None
                else "unwind." + self.exception_target
            )
        # Native entries leave value ownership to their boundary adapter and
        # report errors through their error flag.
        if self.raw:
            return base
        # Builtin algorithms have no Python source frame in the exception chain.
        # Their live activations remain available to native debug diagnostics.
        record_traceback = (
            record_traceback
            and not self.module.program.bootstrap
            and self.lambda_.policy.records_traceback
        )
        owned = (
            *(value for value in self.live.values() if value != failed_value),
            *(f"@unpack.slot.{i}" for i in self.unpacked_remaining_slots),
        )
        if not record_traceback and not owned:
            return base
        key = (base, owned, record_traceback, explicit_raise)
        label = self.landings.get(key)
        if label is None:
            label = self.landings[key] = self.fresh("release")[1:]
        if record_traceback:
            # Landings release owned temporaries; only new failures and explicit
            # raises add source entries. Resuming a caught transfer preserves them.
            sites = self.landing_sites.setdefault(label, {})
            source = (
                None
                if self.vop is None or self.vop.node is None
                else self.vop.node.source
            )
            marker = self.module.source_highlights.marker(
                self.module.code(self.lambda_).filename, source
            )
            sites.setdefault(
                self.current_block(),
                (self.current_line(), self.module.text(marker) if marker else "null"),
            )
        return label

    def current_block(self) -> str:
        """The label of the block being emitted: each failure check starts a
        new block, so the label is a few lines back."""
        for line in reversed(self.lines):
            if not line.startswith(" ") and line.endswith(":"):
                return line[:-1]
        raise AssertionError("no block is being emitted")

    def current_line(self) -> int:
        """The source line of the operation being emitted."""
        node = None if self.vop is None else self.vop.node
        source = None if node is None else node.source
        if source is not None and source.span is not None:
            return source.span.start.line
        return self.module.code(self.lambda_).firstlineno

    def emit_landings(self) -> None:
        """Record source failures (unless resuming a caught transfer), then
        release owned temporaries. A separate address identifies this call
        without exposing the return-value slot to the runtime."""
        if self.landing_sites:
            # The runtime compares and stores this address, never its contents.
            # Keep result storage private so LLVM can promote it to SSA.
            self.lines.insert(
                self.lines.index("entry:") + 1, "  %activation = alloca i8"
            )
        code = self.module.code_descriptor(self.lambda_)
        owned_sets = list(
            dict.fromkeys(
                (base, owned)
                for base, owned, _record, _explicit in self.landings
                if owned
            )
        )
        by_end: dict[tuple[str, int, str], list[tuple[str, ...]]] = {}
        for base, owned in owned_sets:
            by_end.setdefault((base, len(owned), owned[-1]), []).append(owned)
        parent_prefix: dict[tuple[str, tuple[str, ...]], tuple[str, ...]] = {}
        chain_labels: dict[tuple[str, tuple[str, ...]], str] = {}
        for base, owned in owned_sets:
            for index in range(len(owned) - 1, 0, -1):
                candidates = by_end.get((base, index, owned[index - 1]))
                if candidates is None:
                    continue
                prefix = owned[:index]
                if prefix in candidates:
                    parent_prefix[base, owned] = prefix
                    if (base, prefix) not in chain_labels:
                        chain_labels[base, prefix] = self.fresh("release")[1:]
                    break

        def emit_release_step(base: str, owned: tuple[str, ...]) -> None:
            prefix = parent_prefix.get((base, owned))
            if prefix is None:
                for value in owned:
                    self.release(value)
                self.line(f"br label %{base}")
            else:
                for value in owned[len(prefix) :]:
                    self.release(value)
                self.line(f"br label %{chain_labels[base, prefix]}")

        for (
            base,
            owned,
            record_traceback,
            explicit_raise,
        ), label in self.landings.items():
            self.lines.append(label + ":")
            if record_traceback:
                sites = self.landing_sites[label]
                incoming = ", ".join(
                    f"[{site[0]}, %{block}]" for block, site in sites.items()
                )
                line = f"%failed.line.{label}"
                self.line(f"{line} = phi i32 {incoming}")
                incoming_markers = ", ".join(
                    f"[{site[1]}, %{block}]" for block, site in sites.items()
                )
                marker = f"%failed.marker.{label}"
                self.line(f"{marker} = phi ptr {incoming_markers}")
                self.line(
                    f"call void @mule_traceback_here(ptr {code}, i32 {line}, "
                    f"ptr %globals, ptr %activation, i32 {int(explicit_raise)}, ptr {marker})"
                )
            if (base, owned) in chain_labels:
                self.line(f"br label %{chain_labels[base, owned]}")
            else:
                emit_release_step(base, owned)
        for (base, prefix), label in chain_labels.items():
            self.lines.append(label + ":")
            emit_release_step(base, prefix)

    def guard(self, failed: str, failed_value=None) -> str:
        """Branch to the failure path when ``failed``; returns the label of
        the continuation block."""
        continuation = self.fresh("ok")[1:]
        target = self.failure_target(failed_value=failed_value)
        self.line(f"br i1 {failed}, label %{target}, label %{continuation}")
        self.lines.append(continuation + ":")
        return continuation

    def check_pointer(self, value: str) -> str:
        return self.guard(self.operation(f"icmp eq ptr {value}, null"), value)

    def check_status(self, value: str) -> str:
        return self.guard(self.operation(f"icmp slt i32 {value}, 0"))

    def propagate(self, *, record_traceback=True, explicit_raise=False) -> None:
        target = self.failure_target(
            record_traceback=record_traceback, explicit_raise=explicit_raise
        )
        self.line(f"br label %{target}")

    # TN access

    def mark_immortal(self, tn, value: str) -> None:
        if tn.sc in OBJECT and not tn.sc.owned:
            self.immortal_values.add(value)

    def load(self, ref) -> str:
        tn = ref.tn
        if tn in self.registers:
            return self.values[tn]
        value = self.operation(f"load {tn.sc.llvm_type}, ptr {tn.slot}")
        self.record_facts(tn, value)
        return value

    def take(self, ref) -> str:
        """Consume a temporary: the failure paths no longer release it."""
        tn = ref.tn
        if tn.borrowed_from is not None:
            return self.load(ref)
        if tn in self.registers:
            self.live.pop(tn, None)
            return self.values[tn]
        value = self.load(ref)
        if tn.sc in OBJECT:
            self.line(f"store ptr null, ptr {tn.slot}")
        return value

    def store(self, ref, value: str, *, release: bool = True) -> None:
        tn = ref.tn
        if tn in self.registers:
            self.define(tn, value)
            return
        if tn.sc in OBJECT:
            self.publish(ref, value, release=release)
            return
        self.line(f"store {tn.sc.llvm_type} {value}, ptr {tn.slot}")

    def define(self, tn, value: str) -> None:
        # A vop may reuse its result as scratch once it consumed the earlier
        # value; an unconsumed one would need a phi, not a second definition.
        if tn in self.live:
            raise ValueError(f"{tn!r} redefined while it still owns a value")
        self.values[tn] = value
        self.record_facts(tn, value)
        if (
            tn.sc.owned
            and tn.borrowed_from is None
            and value not in self.immortal_values
        ):
            self.live[tn] = value

    def out_pointer(self) -> str:
        """An entry-block slot for the pointer a runtime call writes on every
        return; read it back right after the call."""
        self.uses_out = True
        return "%out"

    def reuse_slot(self) -> str:
        """An entry-block slot holding an inplace operation's local value for
        the slow-path reference-count check, without escaping the local slot."""
        self.uses_reuse = True
        return "%reuse.slot"

    def float_scratch(self) -> str:
        """One entry-block payload extraction slot, reused after each load."""
        self.uses_float_scratch = True
        return "%float.scratch"

    def _has_unchecked_element_contract(self, tn) -> bool:
        return (
            not self.raw
            and not self.checking_element_contract
            and self.vop is not None
            and bool(self.lambda_.policy.safety)
            and self.vop.node in self.context.element_contracts
            and tn.sc in OBJECT
        )

    def publish(self, ref, value: str, *, release: bool = True) -> None:
        tn = ref.tn
        if self._has_unchecked_element_contract(tn):
            if (
                tn not in self.registers
                and tn.sc.owned
                and tn.borrowed_from is None
                and release
            ):
                old = self.operation(f"load ptr, ptr {tn.slot}")
                self.line(f"store ptr null, ptr {tn.slot}")
                self.record_facts(tn, old)
                self.release(old)
            if tn in self.registers:
                if tn in self.live:
                    raise ValueError(f"{tn!r} redefined while it still owns a value")
                self.values[tn] = value
            if (
                tn.sc.owned
                and tn.borrowed_from is None
                and value not in self.immortal_values
            ):
                self.live[tn] = value
            return
        if tn in self.registers:
            self.define(tn, value)
            return
        if tn.sc.owned and tn.borrowed_from is None and release:
            old = self.operation(f"load ptr, ptr {tn.slot}")
            self.line(f"store ptr {value}, ptr {tn.slot}")
            self.record_facts(tn, old)
            self.record_facts(tn, value)
            self.release(old)
            return
        self.line(f"store ptr {value}, ptr {tn.slot}")
        self.record_facts(tn, value)

    def record_facts(self, tn, value: str) -> None:
        self.mark_immortal(tn, value)
        if tn.borrowed_from is not None:
            self.borrowed_values.add(value)
        if tn.private_box is not None:
            self.private_values[value] = tn.private_box
        fact = tn.type or UNKNOWN
        if fact.is_fixnum or fact.is_immortal:
            self.immortal_values.add(value)
            if self.live.get(tn) == value:
                self.live.pop(tn, None)
        elif fact.is_heap_object:
            self.heap_values.add(value)

    def retain(self, value: str) -> str:
        if value in self.immortal_values:
            return value
        layout = self.private_values.get(value)
        if layout is not None:
            operation = f"call ptr @{layout.symbol('private.retain')}(ptr {value})"
        elif value in self.heap_values:
            operation = f"call ptr @mule.heap.retain(ptr {value})"
        else:
            operation = f"call ptr @mule.fast.retain(ptr {value})"
        if value in self.borrowed_values:
            # The acquired owner and the existing borrow have the same pointer,
            # but only the newly acquired reference must be released.
            owned = self.operation(operation)
            if layout is not None:
                self.private_values[owned] = layout
            if value in self.heap_values:
                self.heap_values.add(owned)
            return owned
        self.line(operation)
        return value

    def release(self, value: str) -> None:
        if value.startswith("@unpack.slot."):
            rem_idx = int(value.rsplit(".", 1)[1])
            slot = self.operation(f"getelementptr ptr, ptr %unpack.out, i64 {rem_idx}")
            unpacked_val = self.operation(f"load ptr, ptr {slot}")
            self.line(f"call void @mule.fast.release(ptr {unpacked_val})")
            return
        if not self.owns_reference(value):
            return
        layout = self.private_values.get(value)
        if layout is not None:
            self.line(f"call void @{layout.symbol('private.release')}(ptr {value})")
            return
        if value in self.heap_values:
            self.line(f"call void @mule.heap.release(ptr {value})")
            return
        self.line(f"call void @mule.fast.release(ptr {value})")

    def owns_reference(self, value: str) -> bool:
        """Whether this acquisition may transfer or release a runtime owner."""
        return value not in self.immortal_values and value not in self.borrowed_values

    def refine(self, tn, value: str) -> None:
        """Expose proven representation/range facts after the error check."""
        if self._has_unchecked_element_contract(tn):
            contract = self.context.element_contracts[self.vop.node]
            self.checking_element_contract = True
            self.unpacked_remaining_slots = self.unpacked_ref_remaining(self.vop)
            try:
                contract.accept(vops._ContractAssertion(self, value))
            finally:
                self.unpacked_remaining_slots = ()
                self.checking_element_contract = False
            self.live.pop(tn, None)
            value = self.operation(f"getelementptr i8, ptr {value}, i64 0")
            if tn in self.registers:
                self.define(tn, value)
            else:
                self.line(f"store ptr {value}, ptr {tn.slot}")
                self.record_facts(tn, value)
            if self.component.reads.get(tn):
                self.refine_type(tn.type or UNKNOWN, value)
            return
        self.record_facts(tn, value)
        if self.component.reads.get(tn):
            self.refine_type(tn.type or UNKNOWN, value)

    def refine_type(self, fact: ValueType, value: str, *, nullable=False) -> None:
        if not nullable:
            if fact.is_fixnum or fact.is_immortal:
                self.immortal_values.add(value)
            elif fact.is_heap_object:
                self.heap_values.add(value)
        if not fact.is_fixnum:
            return
        valid = fact.accept(_TypeAssumption(self, value))
        if valid is None:
            return
        if nullable:
            missing = self.operation(f"icmp eq ptr {value}, null")
            valid = self.operation(f"or i1 {missing}, {valid}")
        self.line(f"call void @llvm.assume(i1 {valid})")

    def pointer_result(self, ref, operation: str, consumed=()) -> None:
        value = self.operation(operation)
        self.publish(ref, value)
        for operand in consumed:
            self.release(operand)
        self.check_pointer(value)
        self.refine(ref.tn, value)

    def native_result(self, ref, value: str, consumed=()) -> None:
        self.record_facts(ref.tn, value)
        self.publish(ref, value)
        for operand in consumed:
            self.release(operand)

    def parameter(self, index: int) -> str:
        return f"%parameter.{index}"

    def check_native_receiver(self) -> None:
        """Keep native-class receiver rules independent of body specialization."""
        if self.raw or self.lambda_ in self.module.native_plans:
            return
        layout = self.module.layouts.get(self.lambda_.owner_class)
        if (
            layout is None
            or not self.lambda_.vars
            or self.lambda_ not in layout.methods.values()
        ):
            return
        matched = self.operation(
            f"call i32 @mule_native_instance_matches(ptr %parameter.0, "
            f"ptr {layout.schema}, ptr %globals)"
        )
        wrong = self.operation(f"icmp eq i32 {matched}, 0")
        failed, accepted = self.fresh("receiver.bad")[1:], self.fresh("receiver.ok")[1:]
        self.line(f"br i1 {wrong}, label %{failed}, label %{accepted}")
        self.lines.append(failed + ":")
        self.line("call i32 @mule_native_type_error()")
        self.propagate()
        self.lines.append(accepted + ":")

    def parameter_value(self, index: int, ref) -> str:
        value = self.parameter(index)
        if self.convention is None:
            return value
        sc = self.convention.parameters[index]
        if sc is SIGNED and ref.tn.sc in OBJECT:
            return self.encode(value)
        if sc is DOUBLE and ref.tn.sc is not DOUBLE:
            raise ValueError(
                f"raw float parameter {index} requires double local storage"
            )
        return value

    def return_value(self, operand) -> None:
        if self.convention is None:
            value = self.take(operand)
            if self.raw:
                returned = linked(
                    self.plan.returned, self.plan, "return representation"
                )
                value = returned.prepare_return(self, value)
            llvm_type = operand.tn.sc.llvm_type
        else:
            sc = self.convention.returned
            if sc is DOUBLE:
                value = vops.unbox_float_operand(self, operand)
                consumed = self.take(operand)
                if operand.tn.sc in OBJECT:
                    self.release(consumed)
            else:
                value = self.take(operand)
                if operand.tn.sc in OBJECT:
                    pointer = value
                    value = self.decode(pointer)
                    self.release(pointer)
            llvm_type = sc.llvm_type
        self.line(f"store {llvm_type} {value}, ptr %return")
        self.line("br label %cleanup")

    def numeric_call(self, vop) -> None:
        entry = vop.codegen_info
        convention = linked(entry.context.convention, entry, "convention")
        callable_ref, *arguments = vop.args
        converted = []
        # Keep original operands owned until all fallible extraction and frame
        # entry checks succeed. Their ordinary landing releases them on failure.
        for ref, sc in zip(arguments, convention.parameters, strict=True):
            if sc is DOUBLE:
                converted.append(vops.unbox_float_operand(self, ref))
            elif sc is SIGNED:
                value = self.load(ref)
                converted.append(self.decode(value) if ref.tn.sc in OBJECT else value)
            else:
                converted.append(self.load(ref))
        callee = self.load(callable_ref)
        self.operation(
            f"call ptr @mule_method_entry(ptr {callee}, ptr %callee.globals.slot)"
        )
        callee_globals = self.operation("load ptr, ptr %callee.globals.slot")
        closure = (
            self.operation(f"call ptr @mule_function_closure_of(ptr {callee})")
            if self.module.closure_of(entry.function)
            else None
        )
        status = self.operation("call i32 @mule_enter_frame()")
        self.check_status(status)
        self.uses_numeric_error = True
        self.line("store i1 false, ptr %numeric.error")
        self.take(callable_ref)
        raw_owned = []
        for ref, sc in zip(arguments, convention.parameters, strict=True):
            taken = self.take(ref)
            if sc is not DESCRIPTOR and ref.tn.sc in OBJECT:
                raw_owned.append(taken)
        operands = [f"ptr {callee_globals}", "ptr %numeric.error"]
        if closure is not None:
            operands.append(f"ptr {closure}")
        operands.extend(
            f"{sc.llvm_type} {value}"
            for sc, value in zip(convention.parameters, converted, strict=True)
        )
        value = self.operation(
            f"call {convention.returned.llvm_type} {entry.symbol}({', '.join(operands)})"
        )
        self.line("call void @mule_leave_frame()")
        for owned in raw_owned:
            self.release(owned)
        self.release(callee)
        failed = self.operation("load i1, ptr %numeric.error")
        self.guard(failed)
        result = vop.results[0]
        if convention.returned is SIGNED and result.tn.sc in OBJECT:
            self.native_result(result, self.encode(value))
        else:
            self.store(result, value)

    def local_slot(self, name: str) -> str:
        return self.local_slots[name]

    def local_owned(self, name: str) -> bool:
        tn = self.local_tns_by_name[name]
        return tn.sc is not None and tn.sc.owned and tn.borrowed_from is None

    def dependency(self, var) -> str:
        return self.dependencies[var]

    def owner_slot(self, node) -> str:
        return self.owner_slots[node]

    def argv(self, values) -> None:
        self.max_argv = max(self.max_argv, len(values))
        for index, value in enumerate(values):
            slot = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
            self.line(f"store ptr {value}, ptr {slot}")

    def decode(self, value: str) -> str:
        bits = self.operation(f"ptrtoint ptr {value} to i64")
        return self.operation(f"ashr i64 {bits}, 1")

    def encode(self, value: str) -> str:
        shifted = self.operation(f"shl i64 {value}, 1")
        tagged = self.operation(f"or i64 {shifted}, 1")
        return self.operation(f"inttoptr i64 {tagged} to ptr")

    def constant(self, literal) -> str:
        return literal.accept(self._literal_emitter)

    def borrowed_constant(self, literal) -> str:
        return literal.accept(self._borrowed_literal_emitter)

    def truth(self, tn, value: str) -> str:
        """An ``i1`` for the truth of a value in ``tn``'s representation."""
        sc = tn.sc
        if sc is BOOLEAN:
            return value
        if sc is SIGNED:
            return self.operation(f"icmp ne i64 {value}, 0")
        if sc is DOUBLE:
            return self.operation(f"fcmp une double {value}, 0.0")
        if sc is NULL:
            return "false"
        if tn.primitive_type is NONE_T:
            self.release(value)
            return "false"
        if tn.primitive_type is BOOLEAN_T:
            self.release(value)
            return self.operation(f"icmp eq ptr {value}, @mule.true")
        if (tn.type or UNKNOWN).is_fixnum:
            self.release(value)
            return self.operation(f"icmp ne ptr {value}, inttoptr (i64 1 to ptr)")
        truth = self.operation(f"call i32 @mule.fast.truth(ptr {value})")
        self.release(value)
        self.check_status(truth)
        return self.operation(f"icmp ne i32 {truth}, 0")

    def box(self, tn, value: str) -> str:
        """An owned object for a raw value."""
        sc = tn.sc
        if sc is SIGNED:
            return self.operation(f"call ptr @{INT64_BOX_LAYOUT.symbol()}(i64 {value})")
        if sc is BOOLEAN:
            return self.operation(f"select i1 {value}, ptr @mule.true, ptr @mule.false")
        if sc is NULL:
            return "@mule.none"
        if sc is DOUBLE:
            return self.operation(
                f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
            )
        if sc in (INSTANCE, CLASS, REFERENCE):
            return self.operation(f"call ptr @mule.fast.retain(ptr {value})")
        return value

    def double(self, ref) -> str:
        """A ``double`` for an operand held as a double or a signed word."""
        value = self.take(ref)
        if ref.tn.sc is SIGNED:
            return self.operation(f"sitofp i64 {value} to double")
        return value

    def float_arithmetic(
        self, opcode: str, left: str, right: str, *, check_zero=None
    ) -> str:
        if check_zero is None:
            check_zero = bool(self.lambda_.policy.safety)
        if opcode == "fdiv" and check_zero:
            zero = self.operation(f"fcmp oeq double {right}, 0.0")
            failed, continuation = self.fresh("zero")[1:], self.fresh("nonzero")[1:]
            self.line(f"br i1 {zero}, label %{failed}, label %{continuation}")
            self.lines.append(failed + ":")
            self.line("call i32 @mule_native_zero_division()")
            self.line(f"br label %{self.failure_target()}")
            self.lines.append(continuation + ":")
        return self.operation(f"{opcode} double {left}, {right}")

    def field_address(self, tn, owner: str, name: str) -> str:
        layout = tn.primitive_type.layout
        index = layout.info.fields.index(name)
        slot = self.operation(f"getelementptr i8, ptr {owner}, i64 32")
        payload = self.operation(f"load ptr, ptr {slot}, !invariant.load !{{}}")
        return self.operation(f"getelementptr i64, ptr {payload}, i64 {index}")

    def bind_method(self, receiver: str, name: str) -> str:
        """Materialize an attribute now while the caller retains its receiver root."""
        attribute = self.operation(self.constant(ast.TextLiteral(name)))
        self.check_pointer(attribute)
        receiver = self.operation(f"call ptr @mule.fast.retain(ptr {receiver})")
        return self.sequence_call("get_attribute", [receiver, attribute])

    def take_native_result(self, value: str) -> str:
        """Transfer the adapter's owned raw result after fallible preparation."""
        self.line("store ptr null, ptr %native.result.owner")
        return value

    def arithmetic(self, opcode: str, left: str, right: str) -> str:
        if opcode in ("add", "sub", "mul"):
            if self.lambda_.policy.safety:
                checked = self.operation(
                    f"call {{i64, i1}} @llvm.s{opcode}.with.overflow.i64(i64 {left}, i64 {right})"
                )
                value = self.operation(f"extractvalue {{i64, i1}} {checked}, 0")
                overflow = self.operation(f"extractvalue {{i64, i1}} {checked}, 1")
                failed, continuation = (
                    self.fresh("overflow")[1:],
                    self.fresh("bounded")[1:],
                )
                self.line(f"br i1 {overflow}, label %{failed}, label %{continuation}")
                self.lines.append(failed + ":")
                self.line("call i32 @mule_native_overflow()")
                self.line(f"br label %{self.failure_target('error')}")
                self.lines.append(continuation + ":")
                return value
            return self.operation(f"{opcode} nsw i64 {left}, {right}")
        return self.operation(f"{opcode} i64 {left}, {right}")

    # Calls

    def compiled_call_named(self, name: str, values) -> str:
        """A compiled call of the protocol function named ``name``."""
        return self.compiled_call(self.module.by_name[name], values)

    def compiled_call(
        self, function: Function, values, globals_value="%globals"
    ) -> str:
        """Transfer owned arguments to a compiled body inside a runtime frame.

        The body owns its parameters on every return. Failed frame entry leaves
        them here, so that arm consumes them before returning the error result.
        """
        helper = self.module.compiled_call_helper(function, len(values))
        operands = ", ".join(
            [f"ptr {globals_value}", *(f"ptr {value}" for value in values)]
        )
        return self.operation(f"call ptr {helper}({operands})")

    def sequence_call(self, name: str, values) -> str:
        helper = self.module.protocol_helper(name, len(values))
        operands = ", ".join(f"ptr {value}" for value in values)
        return self.operation(f"call ptr {helper}({operands})")

    def tuple_of(self, result, refs) -> str | None:
        """Build an owned tuple of the given operands, or None when empty.
        The tuple is parked in ``result``'s slot while it is built so error
        cleanup releases it. The returned pointer borrows that slot until taken."""
        if not refs:
            return None
        taken = [self.take(ref) for ref in refs]
        self.argv(taken)
        built = self.operation(
            f"call ptr @mule_sequence_tuple_owned(i64 {len(taken)}, ptr %argv)"
        )
        self.publish(result, built)
        self.check_pointer(built)
        return self.load(result)

    def call_method(
        self,
        info,
        callable_value,
        receiver,
        arguments,
        unpack_arity: int | None = None,
        argument_refs=(),
    ) -> str:
        """Consume receiver/arguments on direct, fallback, and rejected arms."""
        if info.exact_list_append:
            arg = arguments[0]
            move_arg = self.owns_reference(arg) and arg != receiver
            append_fn = (
                "@mule_storage_append_move" if move_arg else "@mule_storage_append"
            )
            result = self.operation(f"call ptr {append_fn}(ptr {receiver}, ptr {arg})")
            self.release(receiver)
            if not move_arg:
                self.release(arg)
            self.immortal_values.add(result)
            return result
        if info.exact_list_pop:
            result = self.operation(f"call ptr @mule_storage_pop(ptr {receiver})")
            self.release(receiver)
            return result
        if info.exact_dict_get:
            owned_receiver = (
                receiver if self.owns_reference(receiver) else self.retain(receiver)
            )
            key = arguments[0]
            owned_key = key if self.owns_reference(key) else self.retain(key)
            if len(arguments) >= 2:
                default = arguments[1]
                owned_default = (
                    default if self.owns_reference(default) else self.retain(default)
                )
            else:
                self.immortal_values.add("@mule.none")
                owned_default = "@mule.none"
            return self.sequence_call(
                "dict_get", [owned_receiver, owned_key, owned_default]
            )
        if info.exact_text_find:
            sub = arguments[0]
            start = arguments[1] if len(arguments) >= 2 else "@mule.none"
            end = arguments[2] if len(arguments) >= 3 else "@mule.none"
            if len(arguments) < 3:
                self.immortal_values.add("@mule.none")
            reverse = 1 if info.name == "rfind" else 0
            result = self.operation(
                f"call ptr @mule_text_find(ptr {receiver}, ptr {sub}, ptr {start}, ptr {end}, i32 {reverse})"
            )
            self.release(receiver)
            for arg in arguments:
                self.release(arg)
            self.immortal_values.add(result)
            return result
        if info.exact_text_split:
            sep = arguments[0]
            maxsplit = arguments[1] if len(arguments) >= 2 else "null"
            result = self.operation(
                f"call ptr @mule_text_split(ptr {receiver}, ptr {sep}, ptr {maxsplit})"
            )
            self.release(receiver)
            for arg in arguments:
                self.release(arg)
            return result
        if info.exact_text_partition:
            sep = arguments[0]
            reverse = 1 if info.name == "rpartition" else 0
            if unpack_arity == 3:
                status = self.operation(
                    f"call i32 @mule_text_partition_into(ptr {receiver}, ptr {sep}, i32 {reverse}, ptr %unpack.out)"
                )
                self.release(receiver)
                self.release(sep)
                self.immortal_values.add("@mule.none")
                ok = self.operation(f"icmp ne i32 {status}, 0")
                result = self.operation(f"select i1 {ok}, ptr @mule.none, ptr null")
                self.immortal_values.add(result)
                return result
            result = self.operation(
                f"call ptr @mule_text_partition(ptr {receiver}, ptr {sep}, i32 {reverse})"
            )
            self.release(receiver)
            self.release(sep)
            return result
        if info.exact_text_affix:
            affix = arguments[0]
            start = arguments[1] if len(arguments) >= 2 else "@mule.none"
            end = arguments[2] if len(arguments) >= 3 else "@mule.none"
            if len(arguments) < 3:
                self.immortal_values.add("@mule.none")
            is_end = 1 if info.name == "endswith" else 0
            result = self.operation(
                f"call ptr @mule_text_affix(ptr {receiver}, ptr {affix}, ptr {start}, ptr {end}, i32 {is_end})"
            )
            self.release(receiver)
            for arg in arguments:
                self.release(arg)
            self.immortal_values.add(result)
            return result
        module = self.module
        if (
            info.target is not None
            and not info.keywords
            and unpack_arity is None
            and info.target.call_order((), len(arguments) + 1) is not None
        ):
            return self.call_resolved_target(
                info, callable_value, receiver, arguments, argument_refs
            )
        candidates = module.method_targets.get(info.name, ())
        functions = tuple(
            target
            for target in candidates
            if any(
                target.call_order(info.keywords, len(arguments), receiver=bound)
                is not None
                for bound in (True, False)
            )
        )
        if not functions:
            if not info.keywords:
                self.argv(arguments)
                buf = "%argv" if arguments else "null"
                return self.operation(
                    f"call ptr @mule_call_resolved_owned(ptr {callable_value}, ptr {receiver}, i64 {len(arguments)}, ptr {buf})"
                )
            helper = module.keyword_call_helper(info.keywords, resolved=True)
            operands = ", ".join(
                [
                    f"ptr {callable_value}",
                    f"ptr {receiver}",
                    *(f"ptr {value}" for value in arguments),
                ]
            )
            return self.operation(f"call ptr {helper}({operands})")
        helper = module.method_call_helper(functions, info.keywords, len(arguments))
        operands = ", ".join(
            [
                f"ptr {callable_value}",
                f"ptr {receiver}",
                *(f"ptr {value}" for value in arguments),
            ]
        )
        return self.operation(f"call ptr {helper}({operands})")

    def argument_guard(self, value: str, expected: ValueType) -> str:
        """Check representation/range without touching or invoking an object."""
        return expected.accept(_ArgumentGuard(self, value))

    def call_resolved_target(
        self, info, callable_value, receiver, arguments, argument_refs
    ) -> str:
        """Call a method analysis resolved from the receiver's class: guard
        that resolution produced that very function, then enter its body
        with the receiver as the first argument, choosing an entry context
        by argument types as a guarded global call does. Any other callable
        takes the general resolved-call path."""
        module = self.module
        target = info.target
        values = (receiver, *arguments)
        supplied = (
            info.receiver_type,
            *((ref.tn.type or UNKNOWN) for ref in argument_refs),
        )
        # Parameters the call leaves to their defaults have no argument type;
        # as for global calls, such calls only take the general body.
        order = target.call_order((), len(values))
        if any(index < 0 for index in order):
            steps = ((None, ()),)
        else:
            steps = _entry_steps(module, target, (supplied[index] for index in order))
        helper = module.direct_call_helper(
            target, (), len(values), steps, receiver_call=True
        )
        operands = ", ".join(
            [
                "ptr %globals",
                f"ptr {callable_value}",
                *(f"ptr {value}" for value in values),
            ]
        )
        return self.operation(f"call ptr {helper}({operands})")

    def call(
        self,
        site,
        callee: str,
        argument_refs,
        arguments,
        *,
        unpack_arity: int | None = None,
        result_contract=None,
    ) -> str:
        """Dispatch once, consuming owned arguments on every arm.

        Direct entries take parameter ownership. External fallback borrows the
        original evaluation-order buffer, so that arm releases its references.
        The separately owned callable stays with the caller across all arms.
        """
        module = self.module
        keywords = site.keywords
        target = site.target
        function = (
            target
            if target is not None
            and target.call_order(keywords, len(arguments)) is not None
            else None
        )
        if function is None:
            helper = (
                module.keyword_call_helper(keywords, resolved=False)
                if keywords
                else module.positional_call_helper(len(arguments))
            )
            operands = ", ".join(
                [f"ptr {callee}", *(f"ptr {value}" for value in arguments)]
            )
            return self.operation(f"call ptr {helper}({operands})")
        order = function.call_order(keywords, len(arguments))
        argument_types = tuple(
            (argument_refs[index].tn.type or UNKNOWN) if index >= 0 else None
            for index in order
        )
        supplied_sources = [source for source in argument_types if source is not None]
        if len(supplied_sources) != len(order):
            steps = ((None, ()),)
        else:
            steps = _entry_steps(module, function, supplied_sources)
        if unpack_arity is not None:
            helper = module.unpacked_direct_call_helper(
                function,
                keywords,
                len(arguments),
                unpack_arity,
                tuple(steps),
                proven=site.proven and not keywords,
                result_contract=result_contract,
            )
            operands = ", ".join(
                [
                    "ptr %globals",
                    f"ptr {callee}",
                    *(f"ptr {value}" for value in arguments),
                    "ptr %unpack.out",
                ]
            )
            return self.operation(f"call ptr {helper}({operands})")
        helper = module.direct_call_helper(
            function,
            keywords,
            len(arguments),
            tuple(steps),
            proven=site.proven and not keywords,
            borrowed_arguments=frozenset(
                index
                for index, ref in enumerate(argument_refs)
                if ref.tn.borrowed_from is not None
                and ref.tn.sc.owned
                and index in site.borrowable_arguments
            ),
        )
        operands = ", ".join(
            [
                "ptr %globals",
                f"ptr {callee}",
                *(f"ptr {value}" for value in arguments),
            ]
        )
        return self.operation(f"call ptr {helper}({operands})")

    # Function emission

    def emit(self) -> str:
        if self.raw:
            return self.emit_raw()
        wrapper = (
            self.wrapper()
            if self.context.id is None
            and not self.unpacked
            and self.convention is None
            and self.context.rejected_fixnum_guard is None
            else ""
        )
        adapter = ""
        # Adapt an object entry only when reboxing preserves the returned
        # object's semantics. Otherwise its ordinary body stays authoritative;
        # the raw entry remains available to native callers.
        linkage = (
            ""
            if self.module.program.exports
            and not self.raw
            and not self.unpacked
            and self.convention is None
            and self.context.id is None
            and self.context.rejected_fixnum_guard is None
            else "internal "
        )
        plan = self.module.native_plans.get(self.lambda_)
        if plan is not None:
            adapt_return = linked(
                plan.returned, plan, "returned representation"
            ).can_adapt_return(self.component, plan)
            adapter = native_adapter(self, plan, linkage, adapt_return=adapt_return)
            self.body = _suffixed(self.body, ".boxed")
            linkage = "internal "
            self.lines = []
        lambda_ = self.lambda_
        parameter_scs = (
            self.convention.parameters
            if self.convention is not None
            else (DESCRIPTOR,) * len(lambda_.vars)
        )
        returned = (
            self.convention.returned if self.convention is not None else DESCRIPTOR
        )
        parameters = ", ".join(
            ["ptr %globals"]
            + (["ptr %error.flag"] if self.convention is not None else [])
            + (["ptr %closure"] if self.module.closure_of(lambda_) else [])
            + [
                f"{sc.llvm_type} %parameter.{index}"
                for index, sc in enumerate(parameter_scs)
            ]
            + (["ptr %tuple.out"] if self.unpacked else [])
        )
        self.lines.append(
            f"define {linkage}{returned.llvm_type} {self.body}({parameters}) {{"
        )
        self.lines.append("entry:")
        if self.context.id is not None:
            for index, fact in enumerate(self.context.parameters):
                if parameter_scs[index] is not DESCRIPTOR:
                    continue
                value = f"%parameter.{index}"
                if fact.is_fixnum or fact.is_immortal:
                    self.immortal_values.add(value)
                self.refine_type(fact, value)
        self.line(f"%return = alloca {returned.llvm_type}")
        self.line(f"store {returned.llvm_type} {returned.zero}, ptr %return")
        for group in (*self.local_slot_groups, *self.value_slot_groups):
            tn = group[0]
            self.line(f"{tn.slot} = alloca {tn.sc.llvm_type}")
            self.line(f"store {tn.sc.llvm_type} {tn.sc.zero}, ptr {tn.slot}")
        for tn in self.range_tns:
            self.line(f"{tn.slot} = alloca [3 x i64]")
            self.line(f"store [3 x i64] zeroinitializer, ptr {tn.slot}")
        if self.range_tns:
            self.line("%range.buffer = alloca [3 x i64]")
        self.entry_end = len(self.lines)
        # Publish only a fully initialized stack record, before any operation
        # that can enter cleanup. Globals remain borrowed from the callable.
        self.line("%frame = alloca {ptr, ptr, ptr}, align 8")
        self.line("%frame.slot = call ptr @mule_frame_head_slot()")
        self.line("%frame.parent = load atomic ptr, ptr %frame.slot acquire, align 8")
        self.line("store ptr %frame.parent, ptr %frame")
        self.line(
            "%frame.code = getelementptr {ptr, ptr, ptr}, ptr %frame, i32 0, i32 1"
        )
        self.line(f"store ptr {self.module.code_descriptor(lambda_)}, ptr %frame.code")
        self.line(
            "%frame.globals = getelementptr {ptr, ptr, ptr}, ptr %frame, i32 0, i32 2"
        )
        self.line("store ptr %globals, ptr %frame.globals")
        self.line("store atomic ptr %frame, ptr %frame.slot release, align 8")
        if lambda_ is self.module.program.entry and self.context.id is None:
            # The runtime creates the main program's globals and enters its
            # top-level body once, before any function it defines can exist.
            self.lines += self.module.link(lambda_.namespace, "%globals", "error", 0)
            self.line(
                f"call void @mule_module_loaded(ptr {self.module.text('__main__')}, ptr %globals)"
            )
        self.line(f"br label %{self.component.blocks[0].label}")
        self.emit_blocks()
        handler_live = None
        for target in dict.fromkeys(
            block_exception_target(block.block) for block in self.component.blocks
        ):
            if target is None:
                continue
            if handler_live is None:
                unwind_tns = {tn for group in self.unwind_slot_groups for tn in group}
                handler_live = _value_tns_live_in(self.component, unwind_tns)
            label = self.component.ir2_blocks[target].label
            self.lines.append("unwind." + label + ":")
            # Values the handler still uses were computed before its region
            # was entered, like the stack below an SBCL catch block; only the
            # temporaries the transfer abandons are released.
            live = handler_live[self.component.ir2_blocks[target]]
            for group in self.unwind_slot_groups:
                tn = group[0]
                if tn.sc not in OBJECT or any(member in live for member in group):
                    continue
                value = self.operation(f"load ptr, ptr {tn.slot}")
                self.line(f"store ptr null, ptr {tn.slot}")
                self.record_facts(tn, value)
                if tn.sc.owned:
                    self.release(value)
            self.line(f"br label %{label}")
        self.emit_landings()
        self.lines.append("error:")
        if self.convention is not None:
            self.line("store i1 true, ptr %error.flag")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        cleanup_groups = [
            group
            for group in (*self.local_slot_groups, *self.value_slot_groups)
            if _needs_cleanup_release(group[0])
        ]
        for group in cleanup_groups:
            tn = group[0]
            loaded = self.operation(f"load ptr, ptr {tn.slot}")
            self.record_facts(tn, loaded)
            self.release(loaded)
        # Suspension can change both the thread and the bottom record's parent.
        # Neither value may be reused from entry when the activation returns.
        self.line("%frame.exit.slot = call ptr @mule_frame_head_slot()")
        self.line("%frame.exit.parent = load ptr, ptr %frame")
        self.line(
            "store atomic ptr %frame.exit.parent, ptr %frame.exit.slot release, align 8"
        )
        value = self.operation(f"load {returned.llvm_type}, ptr %return")
        self.line(f"ret {returned.llvm_type} {value}")
        self.lines.append("}")
        return wrapper + "\n\n" + adapter + "\n\n" + "\n".join(self.lines)

    def emit_blocks(self) -> None:
        for block in self.component.blocks:
            self.lines.append(block.label + ":")
            target = block_exception_target(block.block)
            self.exception_target = (
                self.component.ir2_blocks[target].label if target is not None else None
            )
            for vop in block.vops:
                self.vop = vop
                vop.info.generator(self, vop)
                for tn in self.expiring.get(vop, ()):
                    value = self.live.pop(tn, None)
                    if value is None:
                        continue
                    if vop.info.name in vops.TERMINATORS:
                        raise ValueError(f"{vop!r} ends its block owning {tn!r}")
                    self.release(value)
            if self.live:
                raise ValueError(f"{block!r} ends owning {list(self.live)!r}")
        if self.max_unpack_out:
            self.lines.insert(
                self.entry_end, f"  %unpack.out = alloca [{self.max_unpack_out} x ptr]"
            )
        if self.max_argv:
            self.lines.insert(
                self.entry_end, f"  %argv = alloca [{self.max_argv} x ptr]"
            )
        if self.uses_reuse:
            self.lines.insert(self.entry_end, "  %reuse.slot = alloca ptr")
        if self.uses_out:
            self.lines.insert(self.entry_end, "  %out = alloca ptr")
        if self.uses_float_scratch:
            self.lines.insert(self.entry_end, "  %float.scratch = alloca double")
        if self.uses_numeric_error:
            self.lines.insert(self.entry_end, "  %numeric.error = alloca i1")
            self.lines.insert(self.entry_end, "  %callee.globals.slot = alloca ptr")

    def emit_raw(self) -> str:
        """Native entry: borrowed raw slots and scalars, ownership only at edges."""
        plan = self.plan
        parameters = ["ptr %globals", "ptr %error.flag"]
        parameters.extend(
            f"{rep.llvm_type} %parameter.{index}"
            for index, rep in enumerate(plan.parameters)
        )
        parameters.extend(f"ptr {value}" for value in self.dependencies.values())
        returned = plan.returned
        self.lines = [
            f"define internal {returned.llvm_type} {plan.entry}({', '.join(parameters)}) {{",
            "entry:",
        ]
        self.line(f"%return = alloca {returned.llvm_type}")
        self.line(f"store {returned.llvm_type} {returned.zero}, ptr %return")
        self.line("%call.error = alloca i1")
        for group in (*self.local_slot_groups, *self.value_slot_groups):
            tn = group[0]
            self.line(f"{tn.slot} = alloca {tn.sc.llvm_type}")
            if tn.sc in OBJECT:
                self.line(f"store ptr null, ptr {tn.slot}")
        for slot in self.owner_slots.values():
            self.line(f"{slot} = alloca ptr")
            self.line(f"store ptr null, ptr {slot}")
        self.entry_end = len(self.lines)
        self.line(f"br label %{self.component.blocks[0].label}")
        self.emit_blocks()
        self.emit_landings()
        self.lines.append("error:")
        self.line("store i1 true, ptr %error.flag")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        for slot in self.owner_slots.values():
            owner = self.operation(f"load ptr, ptr {slot}")
            self.line(f"call void @mule.fast.release(ptr {owner})")
        result = self.operation(f"load {returned.llvm_type}, ptr %return")
        self.line(f"ret {returned.llvm_type} {result}")
        self.lines.append("}")
        return "\n".join(self.lines)

    def wrapper_closure(self, lines) -> list[str]:
        """Fetch the running function's closure from the argument view."""
        if not self.module.closure_of(self.lambda_):
            return []
        lines.append("  %closure = call ptr @mule_closure(ptr %args)")
        return ["ptr %closure"]

    def wrapper_body_call(
        self, lines: list[str], param_names: list[str], arguments: str
    ) -> None:
        lambda_ = self.lambda_
        module = self.module
        fixnum_variant = next(
            (v for v in module.variants_of(lambda_) if v.id == "fixnum"), None
        )
        general_body = module.bodies[lambda_]
        if (
            param_names
            and not module.program.bootstrap
            and module.native_plans.get(lambda_) is None
            and fixnum_variant is not None
        ):
            builder = _HelperBuilder(self.module)
            combined = None
            guard = FixnumGuard.from_parameters(fixnum_variant.parameters)
            guard_ok = None if guard is None else guard.emit(builder, param_names)
            for index, (name, expected) in enumerate(
                zip(param_names, fixnum_variant.parameters, strict=True)
            ):
                if expected is UNKNOWN or guard is not None:
                    continue
                if (
                    expected.is_fixnum
                    and expected.minimum == FIXNUM_MIN
                    and expected.maximum == FIXNUM_MAX
                ):
                    bits = f"%fix.bits.{index}"
                    builder.line(f"{bits} = ptrtoint ptr {name} to i64")
                    if combined is None:
                        combined = bits
                    else:
                        next_combined = f"%fix.tags.{index}"
                        builder.line(f"{next_combined} = and i64 {combined}, {bits}")
                        combined = next_combined
                    continue
                checked = expected.accept(_ArgumentGuard(builder, name))
                if checked == "true":
                    continue
                guard_ok = (
                    checked
                    if guard_ok is None
                    else builder.operation(f"and i1 {guard_ok}, {checked}")
                )
            if combined is not None:
                builder.line(f"%fix.tag = and i64 {combined}, 1")
                builder.line("%fix.ok = icmp ne i64 %fix.tag, 0")
                guard_ok = (
                    "%fix.ok"
                    if guard_ok is None
                    else builder.operation(f"and i1 %fix.ok, {guard_ok}")
                )
            if guard_ok is not None:
                if guard is not None:
                    general_body = module.rejected_fixnum_entry(lambda_, guard)
                lines.extend(builder.lines)
                lines.extend(
                    [
                        f"  br i1 {guard_ok}, label %invoke.fixnum, label %invoke.general",
                        "invoke.fixnum:",
                        f"  %result.fixnum = call ptr {module.specialized[lambda_, 'fixnum']}({arguments})",
                        "  ret ptr %result.fixnum",
                        "invoke.general:",
                    ]
                )
        lines.extend(
            [
                f"  %result = call ptr {general_body}({arguments})",
                "  ret ptr %result",
            ]
        )

    def binding_wrapper(self, *, ordinary_view: bool = False) -> str:
        lambda_ = self.lambda_
        module = self.module
        count = len(lambda_.vars)
        kinds = lambda_.signature.kinds
        from mule import ast

        positional = sum(
            kind
            in (
                ast.ParameterKind.POSITIONAL_ONLY,
                ast.ParameterKind.POSITIONAL_OR_KEYWORD,
            )
            for kind in kinds
        )
        default_offsets = {
            index: offset
            for offset, index in enumerate(lambda_.signature.default_parameters)
        }
        rest_idx = next(
            (
                index
                for index, kind in enumerate(kinds)
                if kind == ast.ParameterKind.VAR_POSITIONAL
            ),
            None,
        )
        keywords_idx = next(
            (
                index
                for index, kind in enumerate(kinds)
                if kind == ast.ParameterKind.VAR_KEYWORD
            ),
            None,
        )
        # Without supplied keywords, optional keyword-only parameters use
        # their captured defaults and ``**kwargs`` receives a fresh dictionary.
        fast = all(
            index in default_offsets
            for index, kind in enumerate(kinds)
            if kind == ast.ParameterKind.KEYWORD_ONLY
        )
        required = sum(index not in default_offsets for index in range(positional))
        entry = module.functions[lambda_]
        view_entry = _suffixed(entry, ".view")
        header = (
            f"define internal ptr {view_entry}(ptr %globals, ptr %args) alwaysinline {{"
            if ordinary_view
            else f"define ptr {entry}(ptr %globals, ptr %args) {{"
        )
        lines = [
            header,
            "entry:",
        ]
        binding = _suffixed(module.functions[lambda_], ".binding")
        if fast:
            if not ordinary_view:
                lines += [
                    "  %ordinary = call i1 @mule.argument_view.is_view(ptr %args)",
                    "  br i1 %ordinary, label %view, label %other",
                    "view:",
                    f"  %view.result = tail call ptr {view_entry}(ptr %globals, ptr %args)",
                    "  ret ptr %view.result",
                    "other:",
                ]
            count_helper = (
                "mule.argument_view.positional"
                if ordinary_view
                else "mule_binding_positional"
            )
            take_helper = "mule.argument_view.take" if ordinary_view else "mule_arg"
            default_helper = (
                "mule.argument_view.arg_or_default"
                if ordinary_view
                else "mule_arg_or_default"
            )
            lines += [
                f"  %plain.count = call i32 @{count_helper}(ptr %args, i32 {required}, i32 {positional}, i32 {int(rest_idx is not None)})",
                "  %fits = icmp sge i32 %plain.count, 0",
                "  br i1 %fits, label %plain, label %binding",
                "plain:",
            ]
            if rest_idx is not None:
                lines += [
                    f"  %rest.more = icmp sgt i32 %plain.count, {positional}",
                    f"  %rest.start = select i1 %rest.more, i32 {positional}, i32 %plain.count",
                    f"  %plain.{rest_idx} = call ptr @mule_rest_args(ptr %args, i32 %rest.start)",
                    f"  %plain.missing = icmp eq ptr %plain.{rest_idx}, null",
                    "  br i1 %plain.missing, label %failed, label %plain.ok",
                    "plain.ok:",
                ]
            if keywords_idx is not None:
                build_dict = module.protocol_helper("build_dict", 1)
                lines += [
                    f"  %plain.empty = {module.constant_tuple(ast.Tuple([]))}",
                    "  %plain.empty.missing = icmp eq ptr %plain.empty, null",
                    "  br i1 %plain.empty.missing, label %plain.cleanup, label %plain.dictionary",
                    "plain.dictionary:",
                    f"  %plain.{keywords_idx} = call ptr {build_dict}(ptr %plain.empty)",
                    f"  %plain.dictionary.missing = icmp eq ptr %plain.{keywords_idx}, null",
                    "  br i1 %plain.dictionary.missing, label %plain.cleanup, label %plain.ready",
                    "plain.cleanup:",
                ]
                if rest_idx is not None:
                    lines.append(
                        f"  call void @mule.fast.release(ptr %plain.{rest_idx})"
                    )
                lines += [
                    "  br label %failed",
                    "plain.ready:",
                ]
            for index, kind in enumerate(kinds):
                if kind in (
                    ast.ParameterKind.VAR_POSITIONAL,
                    ast.ParameterKind.VAR_KEYWORD,
                ):
                    continue
                if kind == ast.ParameterKind.KEYWORD_ONLY:
                    lines.append(
                        f"  %plain.{index} = call ptr @mule_binding_default_at(ptr %args, i32 {default_offsets[index]})"
                    )
                elif index < required:
                    lines.append(
                        f"  %plain.{index} = call ptr @{take_helper}(ptr %args, i32 {index})"
                    )
                else:
                    lines.append(
                        f"  %plain.{index} = call ptr @{default_helper}(ptr %args, i32 {index}, i32 {default_offsets[index]})"
                    )
            lines.append("  br label %invoke")
        else:
            lines.append("  br label %binding")
        lines += [
            "binding:",
            f"  %bound.result = tail call ptr {binding}(ptr %globals, ptr %args)",
            "  ret ptr %bound.result",
        ]
        if fast:
            lines.append("invoke:")
            param_names = [f"%plain.{index}" for index in range(count)]
            arguments = ", ".join(
                [
                    "ptr %globals",
                    *self.wrapper_closure(lines),
                    *(f"ptr {name}" for name in param_names),
                ]
            )
            self.wrapper_body_call(lines, param_names, arguments)
        lines += [
            "failed:",
            "  ret ptr null",
            "}",
        ]
        # Keep keyword buffers and variable-sized scratch out of the ordinary
        # positional entry. Do not assume keywords themselves are infrequent.
        if ordinary_view:
            return "\n".join(lines)
        bound_lines = [
            f"define internal ptr {binding}(ptr %globals, ptr %args) noinline {{",
            "entry:",
            "  br label %binding",
            *self.keyword_entry(positional, default_offsets, rest_idx, keywords_idx),
            "invoke:",
        ]
        param_names = [f"%bound.{index}" for index in range(count)]
        arguments = ", ".join(
            [
                "ptr %globals",
                *self.wrapper_closure(bound_lines),
                *(f"ptr {name}" for name in param_names),
            ]
        )
        self.wrapper_body_call(bound_lines, param_names, arguments)
        bound_lines += ["failed:", "  ret ptr null", "}"]
        view = self.binding_wrapper(ordinary_view=True) if fast else ""
        return "\n".join(lines + [""] + bound_lines + [view])

    def keyword_entry(
        self,
        positional: int,
        default_offsets: dict[int, int],
        rest_idx: int | None,
        keywords_idx: int | None,
    ) -> list[str]:
        """The ``binding:`` blocks of the external entry: this function's own
        keyword parser, after SBCL's ``convert-more-entry``. Each supplied
        keyword is matched against this function's parameter names, first by
        the address of the unit's shared name constant and then by text, and
        stored straight into its parameter slot. Unmatched keywords go to
        ``**kwargs``. Ends defining ``%bound.N`` for every parameter, owned,
        and branching from ``%bound.ok`` to ``%invoke``."""
        lambda_ = self.lambda_
        module = self.module
        from mule import ast

        kinds = lambda_.signature.kinds
        names = lambda_.parameters
        count = len(names)
        keyword_params = [
            index
            for index, kind in enumerate(kinds)
            if kind
            in (ast.ParameterKind.POSITIONAL_OR_KEYWORD, ast.ParameterKind.KEYWORD_ONLY)
        ]
        table = module.names(tuple(names[index] for index in keyword_params))
        variadic = {rest_idx, keywords_idx} - {None}
        lines = [
            "binding:",
            f"  %kw.slots = alloca [{count} x ptr]",
            f"  %kw.positional = alloca [{max(positional, 1)} x ptr]",
            "  %kw.count.slot = alloca i64",
            "  %kw.names.slot = alloca ptr",
            "  %kw.values.slot = alloca ptr",
            (
                "  %kw.count = call i64 @mule_binding_view(ptr %args, ptr %kw.positional, "
                f"i64 {positional}, ptr %kw.count.slot, ptr %kw.names.slot, "
                "ptr %kw.values.slot)"
            ),
            "  %kw.view.bad = icmp slt i64 %kw.count, 0",
            "  br i1 %kw.view.bad, label %failed, label %kw.view.ok",
            "kw.view.ok:",
            "  %kw.npos = load i64, ptr %kw.count.slot",
            "  %kw.names = load ptr, ptr %kw.names.slot",
            "  %kw.values = load ptr, ptr %kw.values.slot",
            "  %kw.extra = alloca i8, i64 %kw.count",
        ]
        if rest_idx is None:
            lines += [
                f"  %kw.too.many = icmp sgt i64 %kw.npos, {positional}",
                "  br i1 %kw.too.many, label %kw.too.many.error, label %kw.slots.init",
                "kw.too.many.error:",
                "  call void @mule_binding_error(i32 0, ptr null)",
                "  br label %failed",
            ]
        else:
            lines.append("  br label %kw.slots.init")
        lines.append("kw.slots.init:")
        for index in range(count):
            lines.append(
                f"  %kw.slot.{index} = getelementptr [{count} x ptr], ptr %kw.slots, i64 0, i64 {index}"
            )
            if index < positional:
                lines += [
                    f"  %kw.given.{index} = icmp sgt i64 %kw.npos, {index}",
                    f"  %kw.pos.addr.{index} = getelementptr ptr, ptr %kw.positional, i64 {index}",
                    f"  %kw.pos.{index} = load ptr, ptr %kw.pos.addr.{index}",
                    f"  %kw.init.{index} = select i1 %kw.given.{index}, ptr %kw.pos.{index}, ptr null",
                    f"  store ptr %kw.init.{index}, ptr %kw.slot.{index}",
                ]
            else:
                lines.append(f"  store ptr null, ptr %kw.slot.{index}")
        # Each supplied keyword: find its parameter, then bind or reject it.
        fast = []
        previous = "-1"
        for position, index in enumerate(keyword_params):
            fast += [
                f"  %kw.is.{position} = icmp eq ptr %kw.name, {module.text(names[index])}",
                f"  %kw.fast.{position} = select i1 %kw.is.{position}, i64 {position}, i64 {previous}",
            ]
            previous = f"%kw.fast.{position}"
        cases = " ".join(
            f"i64 {position}, label %kw.bind.{position}"
            for position in range(len(keyword_params))
        )
        lines += [
            "  br label %kw.head",
            "kw.head:",
            "  %kw.k = phi i64 [0, %kw.slots.init], [%kw.k.next, %kw.next]",
            "  %kw.done = icmp eq i64 %kw.k, %kw.count",
            "  br i1 %kw.done, label %kw.end, label %kw.body",
            "kw.body:",
            "  %kw.name.addr = getelementptr ptr, ptr %kw.names, i64 %kw.k",
            "  %kw.name = load ptr, ptr %kw.name.addr",
            "  %kw.extra.addr = getelementptr i8, ptr %kw.extra, i64 %kw.k",
            "  store i8 0, ptr %kw.extra.addr",
            *fast,
            f"  %kw.fast = add i64 {previous}, 0",
            "  %kw.miss = icmp eq i64 %kw.fast, -1",
            "  br i1 %kw.miss, label %kw.slow, label %kw.dispatch",
            "kw.slow:",
            (
                "  %kw.slow.index = call i64 @mule_binding_name_index(ptr %kw.name, "
                f"ptr {table}, i64 {len(keyword_params)})"
            ),
            "  br label %kw.dispatch",
            "kw.dispatch:",
            "  %kw.j = phi i64 [%kw.fast, %kw.body], [%kw.slow.index, %kw.slow]",
            f"  switch i64 %kw.j, label %kw.unknown [{cases}]",
        ]
        for position, index in enumerate(keyword_params):
            lines += [
                f"kw.bind.{position}:",
                f"  %kw.old.{position} = load ptr, ptr %kw.slot.{index}",
                f"  %kw.bound.{position} = icmp ne ptr %kw.old.{position}, null",
                f"  br i1 %kw.bound.{position}, label %kw.duplicate, label %kw.store.{position}",
                f"kw.store.{position}:",
                f"  %kw.value.addr.{position} = getelementptr ptr, ptr %kw.values, i64 %kw.k",
                f"  %kw.value.{position} = load ptr, ptr %kw.value.addr.{position}",
                f"  store ptr %kw.value.{position}, ptr %kw.slot.{index}",
                "  br label %kw.next",
            ]
        lines += [
            "kw.duplicate:",
            "  call void @mule_binding_error(i32 2, ptr %kw.name)",
            "  br label %failed",
            "kw.unknown:",
        ]
        if keywords_idx is not None:
            lines += ["  store i8 1, ptr %kw.extra.addr", "  br label %kw.next"]
        else:
            lines += [
                "  call void @mule_binding_error(i32 1, ptr %kw.name)",
                "  br label %failed",
            ]
        lines += [
            "kw.next:",
            "  %kw.k.next = add i64 %kw.k, 1",
            "  br label %kw.head",
            "kw.end:",
        ]
        # Required parameters nothing supplied.
        for index in range(count):
            if index in variadic or index in default_offsets:
                continue
            lines += [
                f"  %kw.check.{index} = load ptr, ptr %kw.slot.{index}",
                f"  %kw.missing.{index} = icmp eq ptr %kw.check.{index}, null",
                f"  br i1 %kw.missing.{index}, label %kw.missing.error.{index}, label %kw.present.{index}",
                f"kw.missing.error.{index}:",
                f"  call void @mule_binding_error(i32 3, ptr {module.text(names[index])})",
                "  br label %failed",
                f"kw.present.{index}:",
            ]
        # Binding succeeded: acquire owned references.
        owned: list[str] = []

        def fail_block(label: str) -> list[str]:
            return [
                f"{label}:",
                *(f"  call void @mule.fast.release(ptr {value})" for value in owned),
                "  br label %failed",
            ]

        if rest_idx is not None:
            lines += [
                f"  %kw.rest.many = icmp sgt i64 %kw.npos, {positional}",
                f"  %kw.rest.from = select i1 %kw.rest.many, i64 {positional}, i64 %kw.npos",
                "  %kw.rest.start = trunc i64 %kw.rest.from to i32",
                f"  %bound.{rest_idx} = call ptr @mule_rest_args(ptr %args, i32 %kw.rest.start)",
                f"  %kw.rest.bad = icmp eq ptr %bound.{rest_idx}, null",
                "  br i1 %kw.rest.bad, label %failed, label %kw.rest.ok",
                "kw.rest.ok:",
            ]
            owned.append(f"%bound.{rest_idx}")
        if keywords_idx is not None:
            build_dict = module.protocol_helper("build_dict", 1)
            setitem = module.protocol_helper("setitem", 3)
            extra = f"%bound.{keywords_idx}"
            lines += [
                f"  %kw.empty = {module.constant_tuple(ast.Tuple([]))}",
                "  %kw.empty.bad = icmp eq ptr %kw.empty, null",
                "  br i1 %kw.empty.bad, label %kw.dict.fail, label %kw.dict.make",
                "kw.dict.make:",
                f"  {extra} = call ptr {build_dict}(ptr %kw.empty)",
                f"  %kw.dict.bad = icmp eq ptr {extra}, null",
                "  br i1 %kw.dict.bad, label %kw.dict.fail, label %kw.dict.ok",
                *fail_block("kw.dict.fail"),
                "kw.dict.ok:",
            ]
            owned.append(extra)
            lines += [
                "  br label %kw.extra.head",
                "kw.extra.head:",
                "  %kw.e = phi i64 [0, %kw.dict.ok], [%kw.e.next, %kw.extra.next]",
                "  %kw.e.done = icmp eq i64 %kw.e, %kw.count",
                "  br i1 %kw.e.done, label %kw.extra.end, label %kw.extra.test",
                "kw.extra.test:",
                "  %kw.e.flag.addr = getelementptr i8, ptr %kw.extra, i64 %kw.e",
                "  %kw.e.flag = load i8, ptr %kw.e.flag.addr",
                "  %kw.e.set = icmp ne i8 %kw.e.flag, 0",
                "  br i1 %kw.e.set, label %kw.extra.insert, label %kw.extra.next",
                "kw.extra.insert:",
                "  %kw.e.key = call ptr @mule_binding_keyword_text(ptr %args, i64 %kw.e)",
                "  %kw.e.key.bad = icmp eq ptr %kw.e.key, null",
                "  br i1 %kw.e.key.bad, label %kw.extra.fail, label %kw.extra.store",
                "kw.extra.store:",
                "  %kw.e.value.addr = getelementptr ptr, ptr %kw.values, i64 %kw.e",
                "  %kw.e.value.borrowed = load ptr, ptr %kw.e.value.addr",
                "  %kw.e.value = call ptr @mule.fast.retain(ptr %kw.e.value.borrowed)",
                f"  %kw.e.dict = call ptr @mule.fast.retain(ptr {extra})",
                f"  %kw.e.result = call ptr {setitem}(ptr %kw.e.dict, ptr %kw.e.key, ptr %kw.e.value)",
                "  %kw.e.result.bad = icmp eq ptr %kw.e.result, null",
                "  br i1 %kw.e.result.bad, label %kw.extra.fail, label %kw.extra.done",
                "kw.extra.done:",
                "  call void @mule.fast.release(ptr %kw.e.result)",
                "  br label %kw.extra.next",
                "kw.extra.next:",
                "  %kw.e.next = add i64 %kw.e, 1",
                "  br label %kw.extra.head",
                *fail_block("kw.extra.fail"),
                "kw.extra.end:",
            ]
        for index in range(count):
            if index in variadic:
                continue
            if index not in default_offsets:
                lines.append(
                    f"  %bound.{index} = call ptr @mule.fast.retain(ptr %kw.check.{index})"
                )
                owned.append(f"%bound.{index}")
                continue
            offset = default_offsets[index]
            lines += [
                f"  %kw.given.value.{index} = load ptr, ptr %kw.slot.{index}",
                f"  %kw.defaulted.{index} = icmp eq ptr %kw.given.value.{index}, null",
                f"  br i1 %kw.defaulted.{index}, label %kw.default.{index}, label %kw.supplied.{index}",
                f"kw.supplied.{index}:",
                f"  %kw.owned.{index} = call ptr @mule.fast.retain(ptr %kw.given.value.{index})",
                f"  br label %kw.acquired.{index}",
                f"kw.default.{index}:",
                f"  %kw.default.value.{index} = call ptr @mule_binding_default_at(ptr %args, i32 {offset})",
                f"  %kw.default.bad.{index} = icmp eq ptr %kw.default.value.{index}, null",
                f"  br i1 %kw.default.bad.{index}, label %kw.default.fail.{index}, label %kw.acquired.{index}",
                *fail_block(f"kw.default.fail.{index}"),
                f"kw.acquired.{index}:",
                (
                    f"  %bound.{index} = phi ptr [%kw.owned.{index}, %kw.supplied.{index}], "
                    f"[%kw.default.value.{index}, %kw.default.{index}]"
                ),
            ]
            owned.append(f"%bound.{index}")
        lines += ["  br label %bound.ok", "bound.ok:", "  br label %invoke"]
        return lines

    def wrapper(self) -> str:
        """General entry validates an argument view and transfers owned arguments."""
        lambda_ = self.lambda_
        module = self.module
        if lambda_.binding is not None:
            return self.binding_wrapper()
        count = len(lambda_.vars)
        variadic = lambda_.signature.variadic
        fixed = count - int(variadic)
        check = "mule_check_rest_args" if variadic else "mule_check_args"
        lines = [
            f"define ptr {module.functions[lambda_]}(ptr %globals, ptr %args) {{",
            "entry:",
            f"  %status = call i32 @{check}(ptr %args, i32 {fixed})",
            "  %wrong = icmp slt i32 %status, 0",
            "  br i1 %wrong, label %failed, label %checked",
            "checked:",
        ]
        if variadic:
            lines.extend(
                [
                    f"  %arg.{fixed} = call ptr @mule_rest_args(ptr %args, i32 {fixed})",
                    f"  %missing = icmp eq ptr %arg.{fixed}, null",
                    "  br i1 %missing, label %failed, label %rest.ok",
                    "rest.ok:",
                ]
            )
        for index in range(fixed):
            lines.append(f"  %arg.{index} = call ptr @mule_arg(ptr %args, i32 {index})")
        param_names = [f"%arg.{index}" for index in range(count)]
        arguments = ", ".join(
            [
                "ptr %globals",
                *self.wrapper_closure(lines),
                *(f"ptr {name}" for name in param_names),
            ]
        )
        self.wrapper_body_call(lines, param_names, arguments)
        lines.extend(
            [
                "failed:",
                "  ret ptr null",
                "}",
            ]
        )
        return "\n".join(lines)


def native_adapter(
    gen: FunctionGenerator, plan, linkage: str, *, adapt_return=True
) -> str:
    """Boxed entry checks physical boundaries once, then calls a raw entry."""
    parameters = ", ".join(
        ["ptr %globals", *(f"ptr %parameter.{i}" for i in range(len(plan.parameters)))]
    )
    gen.lines = [
        f"define {linkage}ptr {gen.body}({parameters}) {{",
        "entry:",
    ]
    gen.line("%result = alloca ptr")
    gen.line("store ptr null, ptr %result")
    gen.line("%native.error = alloca i1")
    gen.line("store i1 false, ptr %native.error")
    if plan.returned.owns_result:
        gen.line("%native.result.owner = alloca ptr")
        gen.line("store ptr null, ptr %native.result.owner")
    dependencies = []
    slots = []
    for index in range(len(plan.dependencies)):
        slot = f"%dependency.{index}"
        slots.append(slot)
        gen.line(f"{slot} = alloca ptr")
        gen.line(f"store ptr null, ptr {slot}")
    arguments = [
        rep.parameter(gen, f"%parameter.{i}") for i, rep in enumerate(plan.parameters)
    ]
    for index, (var, prediction) in enumerate(plan.dependencies.items()):
        value = gen.operation(
            gen.module.load_global(var, gen.lambda_.policy.safety, speculative=True)
        )
        gen.line(f"store ptr {value}, ptr {slots[index]}")
        gen.check_pointer(value)
        matched = gen.operation(
            f"call i32 @mule_native_class_matches(ptr {value}, ptr {prediction.layout.schema}, ptr %globals)"
        )
        wrong = gen.operation(f"icmp eq i32 {matched}, 0")
        continuation = gen.fresh("layout.ok")[1:]
        gen.line(f"br i1 {wrong}, label %fallback, label %{continuation}")
        gen.lines.append(continuation + ":")
        dependencies.append(value)
    if not adapt_return:
        gen.line("br label %fallback")
        gen.lines.append("fallback:")
        for slot in slots:
            gen.release(gen.operation(f"load ptr, ptr {slot}"))
        boxed = gen.operation(f"call ptr {_suffixed(gen.body, '.boxed')}({parameters})")
        gen.line(f"ret ptr {boxed}")
        gen.emit_landings()
        gen.landings.clear()
        gen.landing_sites.clear()
        gen.lines.extend(["error:", "  br label %cleanup", "cleanup:"])
        for slot in slots:
            gen.release(gen.operation(f"load ptr, ptr {slot}"))
        for index in range(len(plan.parameters)):
            gen.release(f"%parameter.{index}")
        gen.line("ret ptr null")
        gen.lines.append("}")
        return "\n".join(gen.lines)
    call = ["ptr %globals", "ptr %native.error"]
    call.extend(
        f"{rep.llvm_type} {value}"
        for rep, value in zip(plan.parameters, arguments, strict=True)
    )
    call.extend(f"ptr {value}" for value in dependencies)
    result = gen.operation(
        f"call {plan.returned.llvm_type} {plan.entry}({', '.join(call)})"
    )
    if plan.returned.owns_result:
        gen.line(f"store ptr {result}, ptr %native.result.owner")
    failed = gen.operation("load i1, ptr %native.error")
    gen.guard(failed)
    boxed = plan.returned.box(gen, result)
    gen.line(f"store ptr {boxed}, ptr %result")
    gen.line("br label %cleanup")
    if plan.dependencies:
        gen.lines.append("fallback:")
        for slot in slots:
            gen.release(gen.operation(f"load ptr, ptr {slot}"))
        result = gen.operation(
            f"call ptr {_suffixed(gen.body, '.boxed')}({parameters})"
        )
        gen.line(f"ret ptr {result}")
    # The adapter is its own function: its failure edges land here, and the
    # body generated next starts without them.
    gen.emit_landings()
    gen.landings.clear()
    gen.landing_sites.clear()
    gen.lines.append("error:")
    gen.line("br label %cleanup")
    gen.lines.append("cleanup:")
    if plan.returned.owns_result:
        gen.release(gen.operation("load ptr, ptr %native.result.owner"))
    for slot in slots:
        gen.release(gen.operation(f"load ptr, ptr {slot}"))
    for i in range(len(plan.parameters)):
        gen.release(f"%parameter.{i}")
    result = gen.operation("load ptr, ptr %result")
    gen.line(f"ret ptr {result}")
    gen.lines.append("}")
    return "\n".join(gen.lines)


def native_callbacks(module: ModuleGenerator, layout) -> str:
    """Adapt borrowed primitive ABI arguments to owned compiled Mule entries."""
    initializer = layout.methods["__init__"]
    count = len(initializer.vars) - 1
    constructor = [
        f"define internal ptr {layout.constructor}(ptr %globals, ptr %class, i64 %count, ptr %args) {{",
        "entry:",
        f"  %wrong = icmp ne i64 %count, {count}",
        "  br i1 %wrong, label %error, label %checked",
        "checked:",
        "  %owned.class = call ptr @mule.fast.retain(ptr %class)",
    ]
    parameters = ["ptr %globals", "ptr %owned.class"]
    for i in range(count):
        constructor.extend(
            [
                f"  %slot.{i} = getelementptr ptr, ptr %args, i64 {i}",
                f"  %arg.{i} = load ptr, ptr %slot.{i}",
                f"  %owned.{i} = call ptr @mule.fast.retain(ptr %arg.{i})",
            ]
        )
        parameters.append(f"ptr %owned.{i}")
    constructor.extend(
        [
            f"  %result = call ptr {module.bodies[layout.protocols['construct']]}({', '.join(parameters)})",
            "  ret ptr %result",
            "error:",
            "  call i32 @mule_native_type_error()",
            "  ret ptr null",
            "}",
        ]
    )
    if (
        initializer.signature.default_parameters
        or not initializer.signature.positional_body
    ):
        constructor = [
            f"define internal ptr {layout.constructor}(ptr %globals, ptr %class, i64 %count, ptr %args) {{",
            "entry:",
            "  %result = call ptr @mule_construct_arguments(ptr %class, i64 %count, ptr %args)",
            "  ret ptr %result",
            "}",
        ]
    index = [
        "  %shifted = shl i64 %index, 1",
        "  %tagged = or i64 %shifted, 1",
        "  %index.value = inttoptr i64 %tagged to ptr",
        "  %owned.instance = call ptr @mule.fast.retain(ptr %instance)",
    ]
    getter = [
        f"define internal ptr {layout.getter}(ptr %globals, ptr %instance, i64 %index) {{",
        "entry:",
        *index,
        f"  %result = call ptr {module.bodies[layout.protocols['get_field']]}(ptr %globals, ptr %owned.instance, ptr %index.value)",
        "  ret ptr %result",
        "}",
    ]
    setter = [
        f"define internal i32 {layout.setter}(ptr %globals, ptr %instance, i64 %index, ptr %value) {{",
        "entry:",
        *index,
        "  %owned.value = call ptr @mule.fast.retain(ptr %value)",
        f"  %result = call ptr {module.bodies[layout.protocols['set_field']]}(ptr %globals, ptr %owned.instance, ptr %index.value, ptr %owned.value)",
        "  %failed = icmp eq ptr %result, null",
        "  call void @mule.fast.release(ptr %result)",
        "  %status = select i1 %failed, i32 -1, i32 0",
        "  ret i32 %status",
        "}",
    ]
    return "\n\n".join("\n".join(lines) for lines in (constructor, getter, setter))


_ = (FunctionalKind, IMMEDIATE, CLASS)

__all__ = [
    "DECLARATIONS",
    "FunctionGenerator",
    "ModuleGenerator",
    "Program",
    "compile_component",
    "generate",
    "native_adapter",
    "native_callbacks",
]
