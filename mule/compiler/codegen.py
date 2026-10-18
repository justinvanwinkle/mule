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
from dataclasses import dataclass, field
from functools import cache
from typing import TYPE_CHECKING

from mule import ast
from mule.diagnostics import phase

from . import vops
from .ctype import FIXNUM_MAX, FIXNUM_MIN, UNKNOWN, ValueType, ValueTypeVisitor
from .ir1util import block_exception_target, made_function
from .ir2tran import ir2_convert
from .ltn import EntryContext, ltn_analyze
from .node import (
    CallKind,
    CodeInfo,
    FunctionalKind,
    Lambda,
    NodeVisitor,
    Signature,
    linked,
)
from .represent import select_native_plans, select_representations
from .traceback_sites import SourceHighlights
from .vop import (
    BOOLEAN,
    BOOLEAN_T,
    CLASS,
    DESCRIPTOR,
    DOUBLE,
    IMMEDIATE,
    NONE_T,
    NULL,
    RANGE,
    SIGNED,
)

if TYPE_CHECKING:
    from .ir1tran import ImportInfo
    from .native_protocol import Layout

OBJECT = vops.OBJECT
logger = logging.getLogger(__name__)

DECLARATIONS = """declare ptr @mule_literal_int(ptr)
declare ptr @mule_literal_float(double)
declare ptr @mule_literal_text(ptr, i64)
declare ptr @mule_literal_bytes(ptr, i64)
declare ptr @mule_none()
declare ptr @mule_bool(i32)
declare ptr @mule_load_global(ptr, ptr)
declare i32 @mule_store_global(ptr, ptr, ptr)
declare i32 @mule_link_globals(ptr, i64, ptr, ptr, i32)
declare void @mule_traceback_here(ptr, i32, ptr, ptr, i32, ptr)
declare void @mule_frame_push(ptr, ptr)
declare void @mule_frame_pop()
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
declare ptr @mule_text_slice(ptr, ptr, ptr)
declare ptr @mule_text_join(ptr)
declare ptr @mule_storage_hash(ptr)
declare ptr @mule_identity_hash(ptr)
declare ptr @mule_type_of(ptr)
declare ptr @mule_type_name(ptr)
declare ptr @mule_load_local(ptr, ptr)
declare ptr @mule_binary_reuse(i32, ptr, ptr, i32, ptr)
declare ptr @mule_unary(i32, ptr)
declare ptr @mule_compare(i32, ptr, ptr)
declare i32 @mule_truth(ptr)
declare ptr @mule_call(ptr, i64, ptr)
declare ptr @mule_call_keywords(ptr, ptr, i64, ptr, i64, ptr, ptr)
declare ptr @mule_function_defaults(ptr, ptr, ptr, ptr)
declare ptr @mule_function_default(ptr, i32)
declare void @mule_set_argument_constructor(ptr)
declare ptr @mule_construct_arguments(ptr, i64, ptr)
declare i32 @mule_binding_positional(ptr, i32, i32, i32)
declare ptr @mule_arg_or_default(ptr, i32, i32)
declare ptr @mule_binding_default_at(ptr, i32)
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
declare ptr @mule_native_instance_payload(ptr, ptr, ptr)
declare ptr @mule_native_new(ptr)
declare ptr @mule_native_payload(ptr)
declare i32 @mule_unbox_i64(ptr, ptr)
declare ptr @mule_box_i64(i64)
declare i32 @mule_unbox_f64(ptr, ptr)
declare ptr @mule_box_f64(double)
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
declare i32 @mule_check_args(ptr, i32)
declare i32 @mule_assert_integer(ptr, ptr, ptr, ptr)
declare i32 @mule_assert_float(ptr, ptr)
declare i32 @mule_assert_real(ptr, ptr)
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
declare ptr @mule_storage_length(ptr)
declare ptr @mule_storage_read(ptr, ptr)
declare ptr @mule_storage_write(ptr, ptr, ptr)
declare ptr @mule_fast_getitem(ptr, ptr)
declare ptr @mule_fast_getslice(ptr, ptr, ptr, ptr)
declare i32 @mule_fast_setitem(ptr, ptr, ptr)
declare i32 @mule_fast_next_item(ptr, ptr)
declare ptr @mule_storage_append(ptr, ptr)
declare ptr @mule_storage_insert(ptr, ptr, ptr)
declare ptr @mule_storage_delete(ptr, ptr, ptr)
declare ptr @mule_storage_tuple(ptr)
declare ptr @mule_tuple_storage(ptr)
declare i32 @mule_tuple_shape_matches(ptr, i64)
declare i32 @mule_list_shape_matches(ptr, i64)
declare nonnull ptr @mule_tuple_slot(ptr, i64)
declare ptr @mule_storage_compatible(ptr, ptr)
declare ptr @mule_integer_storage(ptr)
declare ptr @mule_class_identity(ptr)
declare ptr @mule_class_parent(ptr)
declare ptr @mule_raised_class(ptr)
declare ptr @mule_exception_targets(ptr)
declare ptr @mule_raise_pending(ptr)
declare ptr @mule_active_exception()
declare ptr @mule_exception_instance(ptr)
declare ptr @mule_invalid_raise()
declare ptr @mule_install_builtin(ptr, ptr)
declare ptr @mule_text_storage(ptr)
declare ptr @mule_stdout_write(ptr)
declare ptr @mule_install_storage(ptr, ptr, ptr, ptr)
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
    for line in vops.helpers(bootstrap=bootstrap).splitlines():
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

    def __init__(self, owned: set, foreign: dict, facts: dict):
        self.owned = owned
        self.foreign = foreign
        self.facts = facts
        self.requested: set[tuple[Function, object]] = set()

    def __getitem__(self, key: tuple[Function, object]) -> str:
        lambda_, context_id = key
        self.requested.add(key)
        facts = self.facts.get(lambda_)
        variants = facts.variants if facts is not None else ()
        for variant, context in enumerate(variants):
            if context.id == context_id:
                name = f"{lambda_.name}.spec.{variant}"
                if lambda_ not in self.owned:
                    self.foreign[name] = ("body", lambda_)
                return _symbol(name)
        raise KeyError((lambda_.name, context_id))


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


def compile_component(lambda_: Lambda, context: EntryContext):
    """Run the IR2 phases for one body (``compile-component`` for a backend
    whose register allocation LLVM performs)."""
    with phase("LTN %s context %s", lambda_.name, context.id, level=logging.DEBUG):
        ltn_analyze(lambda_, context)
    with phase(
        "IR2 conversion %s context %s", lambda_.name, context.id, level=logging.DEBUG
    ):
        component = ir2_convert(lambda_, context)
    with phase(
        "representation selection %s context %s",
        lambda_.name,
        context.id,
        level=logging.DEBUG,
    ):
        select_representations(component)
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
        elif name == "%import":
            info = linked(node.info, node, "import")
            self.imports.setdefault(info.module_name, info)


@dataclass(frozen=True, slots=True)
class TupleConstant:
    literal: ast.Tuple
    slot: str
    entry: str


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
        self.specialized = _SpecializedSymbols(self.owned, self.foreign, self.facts)
        self.by_name = {
            lambda_.name: lambda_ for lambda_ in [*program.external, *lambdas]
        }
        self.layouts = program.layouts
        self.sequence_functions = program.sequence_functions
        with phase("native entry selection", level=logging.DEBUG):
            self.native_plans = select_native_plans(lambdas, self.facts, self.layouts)
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
        self.protocol_helpers: dict[str, str] = {}
        self.auxiliary: list[str] = []
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

    def load_global(self, var, safety: int) -> str:
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
            helper = "live"
        elif var.name in self.program.builtin_names:
            helper = "unchecked"
            self.expected_builtins.setdefault(
                runtime_namespace(var.namespace), set()
            ).add(var.name)
        elif safety == 0:
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
                    sorted(
                        var.name
                        for var in lambda_.environment.closure
                        if not var.name.startswith("$")
                    )
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

    def attribute_cache(self) -> str:
        name = f"@mule.cache.{len(self.auxiliary)}"
        self.auxiliary.append(f"{name} = internal global [27 x i64] zeroinitializer")
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

    def constant_float(self, literal: ast.FloatLiteral) -> str:
        """A unit-owned immutable float, pooled by its exact binary64 bits."""
        key = struct.pack(">d", literal.value)
        existing = self.float_constants.get(key)
        if existing is not None:
            return existing[1]
        index = len(self.float_constants)
        slot = f"@mule.literal.float.slot.{index}"
        entry = (
            f"call ptr @mule.cached.float(ptr {slot}, "
            f"double {vops.double_constant(literal.value)})"
        )
        self.float_constants[key] = slot, entry
        self.auxiliary.append(f"{slot} = internal global ptr null")
        return entry

    def constant_text(self, literal: ast.TextLiteral) -> str:
        """A unit-owned immutable str, pooled by its WTF-8 byte representation."""
        encoded = literal.value.encode("utf-8", "surrogatepass")
        existing = self.text_constants.get(encoded)
        if existing is not None:
            return existing[1]
        index = len(self.text_constants)
        slot = f"@mule.literal.text.slot.{index}"
        data = self.literal_data(encoded)
        entry = (
            f"call ptr @mule.cached.text(ptr {slot}, ptr {data}, i64 {len(encoded)})"
        )
        self.text_constants[encoded] = slot, entry
        self.auxiliary.append(f"{slot} = internal global ptr null")
        return entry

    def constant_bytes(self, literal: ast.BytesLiteral) -> str:
        """A unit-owned immutable bytes object, pooled by its byte sequence."""
        value = literal.value
        existing = self.bytes_constants.get(value)
        if existing is not None:
            return existing[1]
        index = len(self.bytes_constants)
        slot = f"@mule.literal.bytes.slot.{index}"
        data = self.literal_data(value)
        entry = f"call ptr @mule.cached.bytes(ptr {slot}, ptr {data}, i64 {len(value)})"
        self.bytes_constants[value] = slot, entry
        self.auxiliary.append(f"{slot} = internal global ptr null")
        return entry

    def constant_bigint(self, value: int) -> str:
        """A unit-owned immutable arbitrary-precision int literal."""
        existing = self.bigint_constants.get(value)
        if existing is not None:
            return existing[1]
        index = len(self.bigint_constants)
        slot = f"@mule.literal.int.slot.{index}"
        text = self.text(vops.decimal(value))
        entry = f"call ptr @mule.cached.int(ptr {slot}, ptr {text})"
        self.bigint_constants[value] = slot, entry
        self.auxiliary.append(f"{slot} = internal global ptr null")
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

    def constant_tuple(self, literal: ast.Tuple) -> str:
        """Register dependencies before their parent, sharing only typed values."""
        key = literal.accept(_CONSTANT_KEY)
        existing = self.tuple_constants.get(key)
        if existing is not None:
            return existing.entry
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
        else:
            symbol = f"@mule.literal.tuple.{index}"
            entry = f"call ptr {symbol}()"
            self.auxiliary.append(self.tuple_constant_builder(symbol, slot, operations))
        constant = TupleConstant(literal, slot, entry)
        self.tuple_constants[key] = constant
        return constant.entry

    def tuple_constant_builder(
        self, symbol: str, slot: str, operations: list[str]
    ) -> str:
        """Construct the cached tuple on first use; main releases initialized roots."""
        packer = self.tuple_pack_helper(len(operations))
        lines = [
            f"define internal ptr {symbol}() noinline optnone {{",
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
            f"define internal ptr {symbol}(ptr %sequence, ptr %index) {{\n"
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

    def setitem_helper(self) -> str:
        existing = self.protocol_helpers.get("$setitem.fast")
        if existing is not None:
            return existing
        symbol = "@mule.fast.setitem"
        self.protocol_helpers["$setitem.fast"] = symbol
        slow = self.protocol_helper("setitem", 3)
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %sequence, ptr %index, ptr %item) {{\n"
            "entry:\n"
            "  %matched = call i32 @mule_fast_setitem(ptr %sequence, ptr %index, ptr %item)\n"
            "  %valid = icmp ne i32 %matched, 0\n"
            "  br i1 %valid, label %hit, label %slow\n"
            "hit:\n"
            "  ret ptr @mule.none\n"
            "slow:\n"
            "  %owned.seq = call ptr @mule.fast.retain(ptr %sequence)\n"
            "  %owned.idx = call ptr @mule.fast.retain(ptr %index)\n"
            "  %owned.item = call ptr @mule.fast.retain(ptr %item)\n"
            f"  %result = call ptr {slow}(ptr %owned.seq, ptr %owned.idx, ptr %owned.item)\n"
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
            f"define internal i32 {symbol}(ptr %iterator, ptr %out) {{\n"
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
            f"  %result = call ptr @mule_call(ptr %callee, i64 {arity}, ptr {buf})"
        )
        for i in range(arity):
            lines.append(f"  call void @mule.fast.release(ptr %arg.{i})")
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
            f"  %result = call ptr @mule_call_resolved(ptr %callable, ptr %receiver, i64 {arity}, ptr {buf})"
        )
        lines.append("  call void @mule.fast.release(ptr %receiver)")
        for i in range(arity):
            lines.append(f"  call void @mule.fast.release(ptr %arg.{i})")
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
        lines = [
            f"define internal ptr {symbol}({', '.join(params)}) {{",
            "entry:",
            "  %fn.globals.slot = alloca ptr",
            "  %bound = icmp ne ptr %receiver, null",
            "  br i1 %bound, label %resolve, label %fallback",
            "resolve:",
            "  %entry.fn = call ptr @mule_method_entry(ptr %callable, ptr %fn.globals.slot)",
            "  br label %check.0",
        ]
        need_failed_block = False
        for candidate_index, function in enumerate(functions):
            order = function.call_order(keywords, arity, receiver=True)
            next_label = (
                f"check.{candidate_index + 1}"
                if candidate_index + 1 < len(functions)
                else "fallback"
            )
            direct_label = f"direct.{candidate_index}"
            lines.extend(
                [
                    f"check.{candidate_index}:",
                    f"  %match.{candidate_index} = icmp eq ptr %entry.fn, {self.functions[function]}",
                    f"  br i1 %match.{candidate_index}, label %{direct_label}, label %{next_label}",
                    f"{direct_label}:",
                ]
            )
            if (
                function.name == "mule_sequences_mule_fn_ListStorage.append"
                and order
                == (
                    -1,
                    0,
                )
            ):
                lines.extend(
                    [
                        f"  %append.res.{candidate_index} = call ptr @mule_storage_append(ptr %receiver, ptr %arg.0)",
                        "  call void @mule.fast.release(ptr %receiver)",
                        "  call void @mule.fast.release(ptr %arg.0)",
                        f"  ret ptr %append.res.{candidate_index}",
                    ]
                )
                continue
            need_failed_block = True
            entered_label = f"entered.{candidate_index}"
            lines.extend(
                [
                    f"  %frame.{candidate_index} = call i32 @mule_enter_frame()",
                    f"  %rejected.{candidate_index} = icmp slt i32 %frame.{candidate_index}, 0",
                    f"  br i1 %rejected.{candidate_index}, label %failed, label %{entered_label}",
                    f"{entered_label}:",
                    f"  %globals.{candidate_index} = load ptr, ptr %fn.globals.slot",
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
                        f"  %def.{candidate_index}.{pos} = call ptr @mule_function_default(ptr %callable, i32 {-2 - idx})"
                    )
                    lines.append(
                        f"  %own.{candidate_index}.{pos} = call ptr @mule.fast.retain(ptr %def.{candidate_index}.{pos})"
                    )
                    owned.append(f"%own.{candidate_index}.{pos}")
            closure_ops = []
            if self.closure_of(function):
                lines.append(
                    f"  %closure.{candidate_index} = call ptr @mule_function_closure_of(ptr %callable)"
                )
                closure_ops.append(f"ptr %closure.{candidate_index}")
            body_args = ", ".join(
                [
                    f"ptr %globals.{candidate_index}",
                    *closure_ops,
                    *(f"ptr {v}" for v in owned),
                ]
            )
            lines.extend(
                [
                    f"  %direct.res.{candidate_index} = call ptr {self.bodies[function]}({body_args})",
                    "  call void @mule_leave_frame()",
                    f"  ret ptr %direct.res.{candidate_index}",
                ]
            )
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
    ) -> str:
        step_key = tuple(
            (None if ctx is None else ctx.id, needed) for ctx, needed in steps
        )
        key = ("$call.direct", function, keywords, arity, step_key, proven)
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
        builder = _HelperBuilder()
        if proven:
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
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
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
                f"  %matched = call i32 @mule_function_matches(ptr %callee, ptr {self.functions[function]}, ptr %globals)",
                "  %matching = icmp ne i32 %matched, 0",
                "  br i1 %matching, label %direct, label %fallback",
                "direct:",
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
            body = (
                self.specialized[function, context.id]
                if context is not None
                else self.bodies[function]
            )
            body_args = ", ".join(
                ["ptr %globals", *closure_ops, *(f"ptr {v}" for v in owned)]
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
            builder.line(f"  call void @mule.fast.release(ptr %arg.{i})")
        builder.line("  ret ptr null")
        if not proven:
            builder.lines.extend(
                [
                    "fallback:",
                    f"  %fallback.res = call ptr {fallback_helper}({fallback_args})",
                    "  ret ptr %fallback.res",
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
            (UNKNOWN,) * len(lambda_.vars),
            facts.return_type,
        )
        variants = [
            EntryContext(
                context.id,
                context.types,
                context.targets,
                context.parameters,
                context.return_type,
            )
            for context in facts.variants
        ]
        return general, variants

    def emit(self) -> str:
        program = self.program
        functions = []
        variants_by_key = {}
        for lambda_ in program.lambdas:
            general, variants = self.contexts(lambda_)
            functions.append(self.emit_body(lambda_, general))
            for variant in variants:
                variants_by_key[lambda_, variant.id] = (lambda_, variant)
        emitted_variants = set()
        while (
            pending := (self.specialized.requested & variants_by_key.keys())
            - emitted_variants
        ):
            for key in sorted(pending, key=lambda item: (item[0].name, str(item[1]))):
                emitted_variants.add(key)
                lambda_, variant = variants_by_key[key]
                functions.append(self.emit_body(lambda_, variant))
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
            f"define {linkage}ptr {unit_initializer(self.program.unit)}() noinline optnone {{\n"
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
            f"define {linkage}void {unit_finalizer(self.program.unit)}() noinline optnone {{\n"
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

    def emit_body(self, lambda_: Lambda, context: EntryContext) -> str:
        with phase(
            "LLVM body %s context %s", lambda_.name, context.id, level=logging.DEBUG
        ):
            component = compile_component(lambda_, context)
            with phase("VOP generation %s", lambda_.name, level=logging.DEBUG):
                body = FunctionGenerator(self, component).emit()
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
    if tn.sc is not DESCRIPTOR:
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
        key = (tn.sc, tn in owned_set)
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

    def __init__(self) -> None:
        self.lines: list[str] = []
        self.serial = 0

    def line(self, text: str) -> None:
        self.lines.append(text if text.startswith("  ") else "  " + text)

    def operation(self, text: str) -> str:
        self.serial += 1
        value = f"%t.{self.serial}"
        self.lines.append(f"  {value} = {text}")
        return value


class _ArgumentGuard(ValueTypeVisitor):
    """Emit the entry check selected by the parameter fact's own behavior."""

    def __init__(self, gen, value: str):
        self.gen = gen
        self.value = value

    def visit_ValueType(self, fact: ValueType) -> str:
        raise ValueError(f"Unsupported specialization parameter: {fact!r}")

    def visit_UnknownType(self, fact: ValueType) -> str:
        return "true"

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

    def visit_TextType(self, fact: ValueType) -> str:
        if fact.literal is not None:
            return self.visit_ValueType(fact)
        matched = self.gen.operation(f"call ptr @mule_text_storage(ptr {self.value})")
        return self.gen.operation(f"icmp eq ptr {matched}, @mule.true")

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

    def __init__(self, module: ModuleGenerator, component):
        self.module = module
        self._literal_emitter = vops.LiteralEmitter(module)
        self.component = component
        self.lambda_ = component.lambda_
        self.context = component.context
        self.plan = self.context.plan
        self.raw = self.plan is not None
        self.environment = component.environment
        self.immortal_values: set[str] = set()
        self.heap_values: set[str] = set()
        self.lines: list[str] = []
        self.serial = 0
        self.exception_target: str | None = None
        if self.raw:
            self.body = self.plan.entry
        elif self.context.id is not None:
            self.body = module.specialized[self.lambda_, self.context.id]
        else:
            self.body = module.bodies[self.lambda_]
        self.local_slots: dict[str, str] = {}
        self.local_tns_by_name: dict[str, object] = {}
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
        self.entry_end = 0
        self.uses_out = False
        self.uses_reuse = False
        self.uses_float_scratch = False
        self.max_argv = 0
        self.owner_slots: dict[object, str] = {}
        if self.raw:
            self.owner_slots = {
                node: f"%owner.{index}"
                for index, node in enumerate(self.plan.allocations)
            }
            self.dependencies = {
                var: f"%class.{index}"
                for index, var in enumerate(self.plan.dependencies)
            }

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
        owned = tuple(value for value in self.live.values() if value != failed_value)
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

    def emit_landings(self, activation: str = "%return") -> None:
        """Record source failures (unless resuming a caught transfer), then
        release owned temporaries. ``activation`` is a stack slot of this call
        that tells ordinary propagation apart from distinct activations."""
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
                    f"ptr %globals, ptr {activation}, i32 {int(explicit_raise)}, ptr {marker})"
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
        self.mark_immortal(tn, value)
        return value

    def take(self, ref) -> str:
        """Consume a temporary: the failure paths no longer release it."""
        tn = ref.tn
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
        self.mark_immortal(tn, value)
        if tn.sc.owned and value not in self.immortal_values:
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

    def publish(self, ref, value: str, *, release: bool = True) -> None:
        tn = ref.tn
        if tn in self.registers:
            self.define(tn, value)
            return
        if tn.sc.owned and release:
            old = self.operation(f"load ptr, ptr {tn.slot}")
            self.line(f"store ptr {value}, ptr {tn.slot}")
            self.mark_immortal(tn, old)
            self.mark_immortal(tn, value)
            self.release(old)
            return
        self.line(f"store ptr {value}, ptr {tn.slot}")
        self.mark_immortal(tn, value)

    def record_facts(self, tn, value: str) -> None:
        self.mark_immortal(tn, value)
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
        if value in self.heap_values:
            self.line(f"call ptr @mule.heap.retain(ptr {value})")
            return value
        self.line(f"call ptr @mule.fast.retain(ptr {value})")
        return value

    def release(self, value: str) -> None:
        if value in self.immortal_values:
            return
        if value in self.heap_values:
            self.line(f"call void @mule.heap.release(ptr {value})")
            return
        self.line(f"call void @mule.fast.release(ptr {value})")

    def refine(self, tn, value: str) -> None:
        """Expose proven representation/range facts after the error check."""
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

    def local_slot(self, name: str) -> str:
        return self.local_slots[name]

    def local_owned(self, name: str) -> bool:
        return self.local_tns_by_name[name].sc.owned

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
            return self.operation(f"call ptr @mule_box_i64(i64 {value})")
        if sc is BOOLEAN:
            return self.operation(f"select i1 {value}, ptr @mule.true, ptr @mule.false")
        if sc is NULL:
            return "@mule.none"
        if sc is DOUBLE:
            return self.operation(f"call ptr @mule_box_f64(double {value})")
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

    def field_address(self, tn, payload: str, name: str) -> str:
        layout = tn.primitive_type.layout
        index = layout.info.fields.index(name)
        return self.operation(f"getelementptr i64, ptr {payload}, i64 {index}")

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

    def call_method(self, info, callable_value, receiver, arguments) -> str:
        """Consume receiver/arguments on direct, fallback, and rejected arms."""
        if info.exact_list_append:
            result = self.operation(
                f"call ptr @mule_storage_append(ptr {receiver}, ptr {arguments[0]})"
            )
            self.release(receiver)
            self.release(arguments[0])
            self.immortal_values.add(result)
            return result
        module = self.module
        candidates = module.method_targets.get(info.name, ())
        functions = tuple(
            target
            for target in candidates
            if target.call_order(info.keywords, len(arguments), receiver=True)
            is not None
        )
        if not functions:
            helper = (
                module.keyword_call_helper(info.keywords, resolved=True)
                if info.keywords
                else module.resolved_call_helper(len(arguments))
            )
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

    def call(self, site, callee: str, argument_refs, arguments) -> str:
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
            contexts = []
        else:
            contexts = sorted(
                (
                    context
                    for context in module.variants_of(function)
                    if all(
                        source.may_dispatch_fit(expected)
                        for source, expected in zip(
                            supplied_sources, context.parameters, strict=True
                        )
                    )
                ),
                key=lambda context: (
                    sum(
                        0 if source.dispatch_fits(expected) else 1
                        for source, expected in zip(
                            supplied_sources, context.parameters, strict=True
                        )
                    ),
                    sum(fact.dispatch_rank() for fact in context.parameters),
                ),
            )
        steps: list[tuple[EntryContext | None, tuple[ValueType | None, ...]]] = []
        for context in contexts:
            needed = tuple(
                None if source.dispatch_fits(expected) else expected
                for source, expected in zip(
                    supplied_sources, context.parameters, strict=True
                )
            )
            steps.append((context, needed))
            if all(expected is None for expected in needed):
                break
        else:
            steps.append((None, ()))
        helper = module.direct_call_helper(
            function,
            keywords,
            len(arguments),
            tuple(steps),
            proven=site.proven and not keywords,
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
        wrapper = self.wrapper() if self.context.id is None else ""
        adapter = ""
        # Every object entry of a natively planned lambda, general or guarded,
        # becomes the adapter that checks the plan's boundaries and calls the
        # raw entry; its own body remains as the fallback.
        linkage = (
            ""
            if self.module.program.exports and not self.raw and self.context.id is None
            else "internal "
        )
        plan = self.module.native_plans.get(self.lambda_)
        if plan is not None:
            adapter = native_adapter(self, plan, linkage)
            self.body = _suffixed(self.body, ".boxed")
            linkage = "internal "
            self.lines = []
        lambda_ = self.lambda_
        parameters = ", ".join(
            ["ptr %globals"]
            + (["ptr %closure"] if self.module.closure_of(lambda_) else [])
            + [f"ptr %parameter.{index}" for index in range(len(lambda_.vars))]
        )
        attributes = ""
        if lambda_.policy.speed == 0 or (
            self.module.program.bootstrap and lambda_.kind is FunctionalKind.TOPLEVEL
        ):
            attributes = " noinline optnone"
        elif lambda_.policy.debug == 3:
            attributes = " noinline"
        self.lines.append(
            f"define {linkage}ptr {self.body}({parameters}){attributes} {{"
        )
        self.lines.append("entry:")
        if self.context.id is not None:
            for index, fact in enumerate(self.context.parameters):
                value = f"%parameter.{index}"
                if fact.is_fixnum or fact.is_immortal:
                    self.immortal_values.add(value)
                self.refine_type(fact, value)
        self.line("%return = alloca ptr")
        self.line("store ptr null, ptr %return")
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
        if lambda_ is self.module.program.entry and self.context.id is None:
            # The runtime creates the main program's globals and enters its
            # top-level body once, before any function it defines can exist.
            self.lines += self.module.link(lambda_.namespace, "%globals", "error", 0)
            self.line(
                f"call void @mule_module_loaded(ptr {self.module.text('__main__')}, ptr %globals)"
            )
        self.line(
            f"call void @mule_frame_push(ptr {self.module.code_descriptor(lambda_)}, ptr %globals)"
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
                self.mark_immortal(tn, value)
                if tn.sc.owned:
                    self.release(value)
            self.line(f"br label %{label}")
        self.emit_landings()
        self.lines.append("error:")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        cleanup_groups = [
            group
            for group in (*self.local_slot_groups, *self.value_slot_groups)
            if _needs_cleanup_release(group[0])
        ]
        call_release = (
            len(self.component.blocks) > 8
            or len(cleanup_groups) > 8
            or len(self.lines) > 250
        )
        for group in cleanup_groups:
            tn = group[0]
            loaded = self.operation(f"load ptr, ptr {tn.slot}")
            if call_release:
                self.line(f"call void @mule_release(ptr {loaded})")
            else:
                self.release(loaded)
        self.line("call void @mule_frame_pop()")
        value = self.operation("load ptr, ptr %return")
        self.line(f"ret ptr {value}")
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

    def emit_raw(self) -> str:
        """Native entry: borrowed raw slots and scalars, ownership only at edges."""
        plan = self.plan
        lambda_ = self.lambda_
        parameters = ["ptr %globals", "ptr %error.flag"]
        parameters.extend(
            f"{rep.llvm_type} %parameter.{index}"
            for index, rep in enumerate(plan.parameters)
        )
        parameters.extend(f"ptr {value}" for value in self.dependencies.values())
        attributes = " noinline" if lambda_.policy.debug == 3 else ""
        returned = plan.returned
        self.lines = [
            f"define internal {returned.llvm_type} {plan.entry}({', '.join(parameters)}){attributes} {{",
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
        if (
            param_names
            and not module.program.bootstrap
            and module.native_plans.get(lambda_) is None
            and fixnum_variant is not None
        ):
            combined = None
            for index, (name, expected) in enumerate(
                zip(param_names, fixnum_variant.parameters, strict=True)
            ):
                if not expected.is_fixnum:
                    continue
                bits = f"%fix.bits.{index}"
                lines.append(f"  {bits} = ptrtoint ptr {name} to i64")
                if combined is None:
                    combined = bits
                else:
                    next_combined = f"%fix.tags.{index}"
                    lines.append(f"  {next_combined} = and i64 {combined}, {bits}")
                    combined = next_combined
            if combined is not None:
                lines.extend(
                    [
                        f"  %fix.tag = and i64 {combined}, 1",
                        "  %fix.ok = icmp ne i64 %fix.tag, 0",
                        "  br i1 %fix.ok, label %invoke.fixnum, label %invoke.general",
                        "invoke.fixnum:",
                        f"  %result.fixnum = call ptr {module.specialized[lambda_, 'fixnum']}({arguments})",
                        "  ret ptr %result.fixnum",
                        "invoke.general:",
                    ]
                )
        lines.extend(
            [
                f"  %result = call ptr {module.bodies[lambda_]}({arguments})",
                "  ret ptr %result",
            ]
        )

    def binding_wrapper(self) -> str:
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
        # A call without keywords binds here when every parameter it leaves
        # unsupplied has a default; only ``**kwargs`` needs the general binder.
        fast = ast.ParameterKind.VAR_KEYWORD not in kinds and all(
            index in default_offsets
            for index, kind in enumerate(kinds)
            if kind == ast.ParameterKind.KEYWORD_ONLY
        )
        required = sum(index not in default_offsets for index in range(positional))
        lines = [
            f"define ptr {module.functions[lambda_]}(ptr %globals, ptr %args) {{",
            "entry:",
        ]
        if fast:
            lines += [
                f"  %plain.count = call i32 @mule_binding_positional(ptr %args, i32 {required}, i32 {positional}, i32 {int(lambda_.signature.variadic)})",
                "  %fits = icmp sge i32 %plain.count, 0",
                "  br i1 %fits, label %plain, label %binding",
                "plain:",
            ]
            if lambda_.signature.variadic:
                rest_idx = kinds.index(ast.ParameterKind.VAR_POSITIONAL)
                lines += [
                    f"  %rest.more = icmp sgt i32 %plain.count, {positional}",
                    f"  %rest.start = select i1 %rest.more, i32 {positional}, i32 %plain.count",
                    f"  %plain.{rest_idx} = call ptr @mule_rest_args(ptr %args, i32 %rest.start)",
                    f"  %plain.missing = icmp eq ptr %plain.{rest_idx}, null",
                    "  br i1 %plain.missing, label %failed, label %plain.ok",
                    "plain.ok:",
                ]
            for index, kind in enumerate(kinds):
                if kind == ast.ParameterKind.VAR_POSITIONAL:
                    continue
                if kind == ast.ParameterKind.KEYWORD_ONLY:
                    lines.append(
                        f"  %plain.{index} = call ptr @mule_binding_default_at(ptr %args, i32 {default_offsets[index]})"
                    )
                elif index < required:
                    lines.append(
                        f"  %plain.{index} = call ptr @mule_arg(ptr %args, i32 {index})"
                    )
                else:
                    lines.append(
                        f"  %plain.{index} = call ptr @mule_arg_or_default(ptr %args, i32 {index}, i32 {default_offsets[index]})"
                    )
            lines.append("  br label %invoke")
        else:
            lines.append("  br label %binding")
        binder = module.bodies[
            linked(module.program.argument_binder, module.program, "argument binder")
        ]
        lines += [
            "binding:",
            "  %private = call ptr @mule_protocol_globals()",
            "  %owned.view = call ptr @mule.fast.retain(ptr %args)",
            f"  %signature = {module.constant_tuple(lambda_.binding)}",
            f"  %bound = call ptr {binder}(ptr %private, ptr %owned.view, ptr %signature)",
            "  %unbound = icmp eq ptr %bound, null",
            "  br i1 %unbound, label %failed, label %bound.ok",
            "bound.ok:",
        ]
        for index in range(count):
            lines.append(
                f"  %bound.{index} = call ptr @mule_arg(ptr %bound, i32 {index})"
            )
        lines += [
            "  call void @mule.fast.release(ptr %bound)",
            "  br label %invoke",
            "invoke:",
        ]
        plain_pred = "plain.ok" if (fast and lambda_.signature.variadic) else "plain"
        for index in range(count):
            if fast:
                lines.append(
                    f"  %parameter.{index} = phi ptr [%plain.{index}, %{plain_pred}], [%bound.{index}, %bound.ok]"
                )
            else:
                lines.append(
                    f"  %parameter.{index} = getelementptr i8, ptr %bound.{index}, i64 0"
                )
        param_names = [f"%parameter.{index}" for index in range(count)]
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
        return "\n".join(lines)

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


def native_adapter(gen: FunctionGenerator, plan, linkage: str) -> str:
    """Boxed entry checks physical boundaries once, then calls a raw entry."""
    parameters = ", ".join(
        ["ptr %globals", *(f"ptr %parameter.{i}" for i in range(len(plan.parameters)))]
    )
    attributes = " noinline" if plan.function.policy.debug == 3 else ""
    gen.lines = [
        f"define {linkage}ptr {gen.body}({parameters}){attributes} {{",
        "entry:",
    ]
    gen.line("%result = alloca ptr")
    gen.line("store ptr null, ptr %result")
    gen.line("%native.error = alloca i1")
    gen.line("store i1 false, ptr %native.error")
    dependencies = []
    slots = []
    for index in range(len(plan.dependencies)):
        slot = f"%dependency.{index}"
        slots.append(slot)
        gen.line(f"{slot} = alloca ptr")
        gen.line(f"store ptr null, ptr {slot}")
    for index, (var, prediction) in enumerate(plan.dependencies.items()):
        value = gen.operation(gen.module.load_global(var, gen.lambda_.policy.safety))
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
    arguments = [
        rep.parameter(gen, f"%parameter.{i}") for i, rep in enumerate(plan.parameters)
    ]
    call = ["ptr %globals", "ptr %native.error"]
    call.extend(
        f"{rep.llvm_type} {value}"
        for rep, value in zip(plan.parameters, arguments, strict=True)
    )
    call.extend(f"ptr {value}" for value in dependencies)
    result = gen.operation(
        f"call {plan.returned.llvm_type} {plan.entry}({', '.join(call)})"
    )
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
    gen.emit_landings(activation="%result")
    gen.landings.clear()
    gen.landing_sites.clear()
    gen.lines.append("error:")
    gen.line("br label %cleanup")
    gen.lines.append("cleanup:")
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
