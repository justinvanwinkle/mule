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
import struct
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from mule import ast
from mule.diagnostics import phase

from . import vops
from .ctype import FIXNUM_MAX, FIXNUM_MIN, UNKNOWN, ValueType, ValueTypeVisitor
from .ir1util import block_exception_target, class_method_definitions, known_calls
from .ir2tran import ir2_convert
from .ltn import EntryContext, ltn_analyze
from .node import CodeInfo, FunctionalKind, Lambda, Signature, linked
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
declare void @mule_safepoint()
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
declare i32 @mule_binding_plain(ptr, i32, i32)
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
declare ptr @mule_call_resolved(ptr, ptr, i64, ptr)
declare i32 @mule_check_args(ptr, i32)
declare i32 @mule_assert_integer(ptr, ptr, ptr, ptr)
declare i32 @mule_assert_float(ptr, ptr)
declare i32 @mule_assert_real(ptr, ptr)
declare ptr @mule_arg(ptr, i32)
declare ptr @mule_retain(ptr returned)
declare void @mule_release(ptr)
declare i32 @mule_enter_frame()
declare void @mule_leave_frame()
declare i32 @mule_function_matches(ptr, ptr, ptr)
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
declare i32 @mule_fast_setitem(ptr, ptr, ptr)
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
    external_methods: dict[str, ForeignFunction] = field(default_factory=dict)
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

    def __init__(self, module, suffix):
        self.module = module
        self.suffix = suffix

    def __getitem__(self, lambda_: Function) -> str:
        return self.module.reference(
            lambda_, f"{lambda_.name}.{self.suffix}", self.suffix
        )


class _SpecializedSymbols:
    """The guarded entry of a lambda for one of its entry contexts."""

    def __init__(self, module):
        self.module = module
        self.requested: set[tuple[Function, object]] = set()

    def __getitem__(self, key: tuple[Function, object]) -> str:
        lambda_, context_id = key
        self.requested.add(key)
        variant = self.module.variant_index(lambda_, context_id)
        return self.module.reference(lambda_, f"{lambda_.name}.spec.{variant}", "body")


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


class _NestedTuples(ast.Visitor):
    """Registers the tuple constants nested in a tuple constant first."""

    def __init__(self, module):
        self.module = module

    def visit_Tuple(self, node):
        self.module.constant_tuple(node)


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
        self.facts = {**program.external_facts, **program.facts}
        # Other units' functions this unit refers to, by symbol name.
        self.foreign: dict[str, tuple[str, Function]] = {}
        self.functions = _Symbols(self, "fn")
        self.bodies = _Symbols(self, "body")
        self.specialized = _SpecializedSymbols(self)
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
        self.method_targets: dict[str, Function] = {}
        self.imports: dict[str, object] = {}
        if program.module_import is not None:
            self.imports[program.module_import.module_name] = program.module_import
        for lambda_ in lambdas:
            self._collect(lambda_)
        # Other units' methods are predictions too, after this unit's own.
        for method_name, target in program.external_methods.items():
            self.method_targets.setdefault(method_name, target)

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

    def _collect_methods(self, lambda_: Lambda) -> None:
        """Native method predictions; calls still check callable identity."""
        for methods in class_method_definitions(lambda_).values():
            for method_name, target in methods.items():
                self.method_targets.setdefault(method_name, target)

    def _collect(self, lambda_: Lambda) -> None:
        """Method predictions and the imported modules in first-use order."""
        self._collect_methods(lambda_)
        for call in known_calls(lambda_, "%import"):
            info = linked(call.info, call, "import")
            self.imports.setdefault(info.module_name, info)

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
        return (
            info.native
            or info.missing
            or info.initializer in {lambda_.name for lambda_ in self.owned}
        )

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
        self.auxiliary.append(f"{name} = internal global [3 x i64] zeroinitializer")
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
        return (
            f"getelementptr inbounds ([{len(value) + 1} x i8], "
            f"ptr {self.data[value]}, i64 0, i64 0)"
        )

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
        entry = f"@mule.literal.float.{index}"
        self.float_constants[key] = slot, entry
        self.auxiliary.extend(
            [
                f"{slot} = internal global ptr null",
                (
                    f"define internal ptr {entry}() {{\n"
                    "entry:\n"
                    f"  %cached = load ptr, ptr {slot}\n"
                    "  %present = icmp ne ptr %cached, null\n"
                    "  br i1 %present, label %ready, label %build\n"
                    "build:\n"
                    f"  %built = call ptr @mule_literal_float(double {vops.double_constant(literal.value)})\n"
                    f"  store ptr %built, ptr {slot}\n"
                    "  br label %ready\n"
                    "ready:\n"
                    "  %value = phi ptr [%cached, %entry], [%built, %build]\n"
                    "  %owned = call ptr @mule_retain(ptr %value)\n"
                    "  ret ptr %owned\n"
                    "}"
                ),
            ]
        )
        return entry

    def constant_tuple(self, literal: ast.Tuple) -> str:
        """Register dependencies before their parent, sharing only typed values."""
        key = literal.accept(_ConstantKey())
        existing = self.tuple_constants.get(key)
        if existing is not None:
            return existing.entry
        nested = _NestedTuples(self)
        for element in literal.elements:
            element.accept(nested)
        index = len(self.tuple_constants)
        constant = TupleConstant(
            literal, f"@mule.literal.slot.{index}", f"@mule.literal.tuple.{index}"
        )
        self.tuple_constants[key] = constant
        self.auxiliary.extend(
            [
                f"{constant.slot} = internal global ptr null",
                self.tuple_constant_builder(constant),
            ]
        )
        return constant.entry

    def tuple_constant_builder(self, constant: TupleConstant) -> str:
        """Construct the cached tuple on first use; main releases initialized roots."""
        elements = constant.literal.elements
        operations = [element.accept(vops.LiteralEmitter(self)) for element in elements]
        lines = [
            f"define internal ptr {constant.entry}() noinline optnone {{",
            "entry:",
            f"  %cached = load ptr, ptr {constant.slot}",
            "  %present = icmp ne ptr %cached, null",
            "  br i1 %present, label %ready, label %build",
            "build:",
        ]
        if not elements:
            lines.append(
                "  %tuple = call ptr @mule_sequence_tuple_owned(i64 0, ptr null)"
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
            lines.append(
                f"  %tuple = call ptr @mule_sequence_tuple_owned(i64 {len(elements)}, ptr {items})"
            )
        else:
            count = len(elements)
            lines.append(f"  %argv = alloca [{count} x ptr]")
            for position, operation in enumerate(operations):
                value = f"%value.{position}"
                address = f"%arg.{position}"
                lines.extend(
                    [
                        f"  {value} = {operation}",
                        f"  {address} = getelementptr [{count} x ptr], ptr %argv, i64 0, i64 {position}",
                        f"  store ptr {value}, ptr {address}",
                    ]
                )
            lines.append(
                f"  %tuple = call ptr @mule_sequence_tuple_owned(i64 {count}, ptr %argv)"
            )
        lines.extend(
            [
                f"  store ptr %tuple, ptr {constant.slot}",
                "  %tuple.bad = icmp eq ptr %tuple, null",
                "  br i1 %tuple.bad, label %failed.tuple, label %ready",
                "ready:",
                "  %result = phi ptr [%cached, %entry], [%tuple, %build]",
                "  %owned = call ptr @mule_retain(ptr %result)",
                "  ret ptr %owned",
                "failed.tuple:",
                "  ret ptr null",
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
        operands = ", ".join(
            ["ptr %globals", *(f"ptr %arg.{i}" for i in range(arity))]
        )
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
            "failed:\n"
            + (releases + "\n" if releases else "")
            + "  ret ptr null\n"
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
            "  call void @mule.fast.release(ptr %sequence)\n"
            "  call void @mule.fast.release(ptr %index)\n"
            "  ret ptr %fast\n"
            "slow:\n"
            f"  %result = call ptr {slow}(ptr %sequence, ptr %index)\n"
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
            "  call void @mule.fast.release(ptr %sequence)\n"
            "  call void @mule.fast.release(ptr %index)\n"
            "  call void @mule.fast.release(ptr %item)\n"
            "  %none = call ptr @mule_retain(ptr @mule.none)\n"
            "  ret ptr %none\n"
            "slow:\n"
            f"  %result = call ptr {slow}(ptr %sequence, ptr %index, ptr %item)\n"
            "  ret ptr %result\n"
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
        globals_ = []
        for value, name in self.data.items():
            escaped = "".join(f"\\{byte:02X}" for byte in value + b"\0")
            globals_.append(
                f"{name} = private unnamed_addr constant "
                f'[{len(value) + 1} x i8] c"{escaped}"'
            )
        foreign_decls = self.foreign_declarations()
        body_sections = [
            section
            for section in (foreign_decls, *globals_, *self.auxiliary, *functions)
            if section
        ]
        body_text = "\n\n".join(body_sections)
        intrinsics_lines = []
        helper_defs = []
        current_symbol = None
        current_lines: list[str] = []
        for line in vops.helpers(bootstrap=program.bootstrap).splitlines():
            if current_symbol is not None:
                current_lines.append(line)
                if line == "}":
                    helper_defs.append((current_symbol, "\n".join(current_lines)))
                    current_symbol = None
                    current_lines = []
            elif line.startswith("define "):
                current_symbol = line.split("(")[0].split()[-1]
                current_lines = [line]
            elif line.strip():
                intrinsics_lines.append(line)
        kept_helpers: list[str] = []
        kept_symbols: set[str] = set()
        search_text = body_text
        changed = True
        while changed:
            changed = False
            for symbol, chunk in helper_defs:
                if symbol not in kept_symbols and f"{symbol}(" in search_text:
                    kept_symbols.add(symbol)
                    kept_helpers.append(chunk)
                    search_text += "\n" + chunk
                    changed = True
        kept_decls = []
        for line in [*DECLARATIONS.splitlines(), *intrinsics_lines]:
            stripped = line.strip()
            if not stripped or stripped.startswith(";"):
                continue
            symbol = (
                stripped.split()[0]
                if stripped.startswith("@")
                else stripped.split("(")[0].split()[-1]
            )
            if symbol in search_text:
                kept_decls.append(line)
        return (
            "\n\n".join(
                [
                    "\n".join(kept_decls),
                    *kept_helpers,
                    *body_sections,
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


def _value_tns_live_in(component, candidates) -> dict:
    """Backward liveness of the cross-block temporaries in ``candidates``:
    for each IR2 block, those read on some path from its start before being
    written. Error edges count as successors."""
    uses, defs, successors = {}, {}, {}
    for block in component.blocks:
        used, defined = set(), set()
        for vop in block.vops:
            for ref in vop.args:
                if ref.tn in candidates and ref.tn not in defined:
                    used.add(ref.tn)
            for ref in vop.results:
                if ref.tn in candidates:
                    defined.add(ref.tn)
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


def _block_local_last_vop(tn, positions):
    """The last vop using a temporary that is written once and read only by
    later vops of the writer's block, or None when it needs a slot."""
    if len(tn.writes) != 1:
        return None
    writer = tn.writes[0].vop
    if writer not in positions:
        return None
    last = writer
    for ref in tn.reads:
        reader = ref.vop
        if (
            reader not in positions
            or reader.block is not writer.block
            or positions[reader] <= positions[writer]
        ):
            return None
        if positions[reader] > positions[last]:
            last = reader
    return last


class FunctionGenerator:
    """Emit one LLVM function for one IR2 component (one body)."""

    def __init__(self, module: ModuleGenerator, component):
        self.module = module
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
        self.labels = {block: block.label for block in component.blocks}
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
            vop: index
            for block in component.blocks
            for index, vop in enumerate(block.vops)
        }
        for tn in component.tns:
            if tn.primitive_type is not None and tn.sc is RANGE:
                self.range_tns.append(tn)
                tn.slot = f"%range.{len(self.range_tns) - 1}"
            elif tn.leaf is not None and tn.leaf.expression_temporary:
                # A variable's slot is loaded and stored by name, so an
                # expression temporary stays in a slot, released when an
                # exception transfers out of the expression.
                self.value_tns.append(tn)
                tn.slot = f"%value.{len(self.value_tns) - 1}"
            elif tn.leaf is None and not (
                tn.name is not None and tn.name.startswith("$")
            ):
                # Compiler temporaries. The leafless "$cell:"/"$copy:" TNs
                # holding captures are locals, below.
                last = _block_local_last_vop(tn, positions)
                if last is not None:
                    tn.slot = None
                    self.registers.add(tn)
                    self.expiring.setdefault(last, []).append(tn)
                else:
                    self.value_tns.append(tn)
                    tn.slot = f"%value.{len(self.value_tns) - 1}"
            else:
                self.local_tns.append(tn)
                tn.slot = f"%local.{len(self.local_tns) - 1}"
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
        self.max_args = max(
            (len(vop.args) for block in component.blocks for vop in block.vops),
            default=0,
        )

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
            for value in owned:
                self.release(value)
            self.line(f"br label %{base}")

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
            self.line(f"call ptr @mule_retain(ptr {value})")
            return value
        self.line(f"call ptr @mule.fast.retain(ptr {value})")
        return value

    def release(self, value: str) -> None:
        if value in self.immortal_values:
            return
        if value in self.heap_values:
            self.line(f"call void @mule_release(ptr {value})")
            return
        self.line(f"call void @mule.fast.release(ptr {value})")

    def refine(self, tn, value: str) -> None:
        """Expose proven representation/range facts after the error check."""
        self.record_facts(tn, value)
        if tn.reads:
            self.refine_type(tn.type or UNKNOWN, value)

    def refine_type(self, fact: ValueType, value: str, *, nullable=False) -> None:
        if not nullable:
            if fact.is_fixnum or fact.is_immortal:
                self.immortal_values.add(value)
            elif fact.is_heap_object:
                self.heap_values.add(value)
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
        return literal.accept(vops.LiteralEmitter(self.module))

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
        body = self.module.bodies[function]
        status = self.operation("call i32 @mule_enter_frame()")
        failed, entered, join = (
            self.fresh(label)[1:]
            for label in ("protocol.failed", "protocol.entered", "protocol.join")
        )
        bad = self.operation(f"icmp slt i32 {status}, 0")
        self.line(f"br i1 {bad}, label %{failed}, label %{entered}")
        self.lines.append(failed + ":")
        for value in values:
            self.release(value)
        self.line(f"br label %{join}")
        self.lines.append(entered + ":")
        operands = ", ".join(
            [f"ptr {globals_value}", *(f"ptr {value}" for value in values)]
        )
        result = self.operation(f"call ptr {body}({operands})")
        self.line("call void @mule_leave_frame()")
        self.line(f"br label %{join}")
        self.lines.append(join + ":")
        return self.operation(f"phi ptr [null, %{failed}], [{result}, %{entered}]")

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

    def keyword_expression(self, callable_value, receiver, values, keywords):
        positional = [
            value for value, name in zip(values, keywords, strict=True) if name is None
        ]
        named = [
            (name, value)
            for value, name in zip(values, keywords, strict=True)
            if name is not None
        ]
        self.argv([*positional, *(value for name, value in named)])
        kw_values = self.operation(
            f"getelementptr ptr, ptr %argv, i64 {len(positional)}"
        )
        names = self.module.names(tuple(name for name, value in named))
        return (
            f"call ptr @mule_call_keywords(ptr {callable_value}, ptr {receiver}, "
            f"i64 {len(positional)}, ptr %argv, i64 {len(named)}, ptr {names}, ptr {kw_values})"
        )

    def call_method(self, info, callable_value, receiver, arguments) -> str:
        """Consume receiver/arguments on direct, fallback, and rejected arms."""
        module = self.module
        target = module.method_targets.get(info.name)
        function = (
            target
            if target is not None
            and target.call_order(info.keywords, len(arguments), receiver=True)
            is not None
            else None
        )
        fallback_call = (
            f"call ptr @mule_call_resolved(ptr {callable_value}, ptr {receiver}, "
            f"i64 {len(arguments)}, ptr %argv)"
        )
        if info.keywords:
            fallback_call = self.keyword_expression(
                callable_value, receiver, arguments, info.keywords
            )
        if function is None:
            result = self.operation(fallback_call)
            for value in (receiver, *arguments):
                self.release(value)
            return result
        order = function.call_order(info.keywords, len(arguments), receiver=True)
        normalized = [
            receiver if index == -1 else arguments[index] if index >= 0 else None
            for index in order
        ]
        matched = self.operation(
            f"call i32 @mule_function_matches(ptr {callable_value}, "
            f"ptr {module.functions[function]}, ptr %globals)"
        )
        identity = self.operation(f"icmp ne i32 {matched}, 0")
        bound = self.operation(f"icmp ne ptr {receiver}, null")
        valid = self.operation(f"and i1 {identity}, {bound}")
        direct, fallback, entered, failed, join = (
            self.fresh(label)[1:]
            for label in (
                "method.direct",
                "method.fallback",
                "method.entered",
                "method.failed",
                "method.join",
            )
        )
        self.line(f"br i1 {valid}, label %{direct}, label %{fallback}")
        self.lines.append(direct + ":")
        frame = self.operation("call i32 @mule_enter_frame()")
        rejected = self.operation(f"icmp slt i32 {frame}, 0")
        self.line(f"br i1 {rejected}, label %{failed}, label %{entered}")
        self.lines.append(failed + ":")
        for value in (receiver, *arguments):
            self.release(value)
        self.line(f"br label %{join}")
        self.lines.append(entered + ":")
        owned = []
        for index, value in zip(order, normalized, strict=True):
            if value is None:
                borrowed = self.operation(
                    f"call ptr @mule_function_default(ptr {callable_value}, i32 {-2 - index})"
                )
                value = self.operation(f"call ptr @mule.fast.retain(ptr {borrowed})")
            owned.append(value)
        operands = ", ".join(
            [
                "ptr %globals",
                *module.closure_operand(self, function, callable_value),
                *(f"ptr {value}" for value in owned),
            ]
        )
        result = self.operation(f"call ptr {module.bodies[function]}({operands})")
        self.line("call void @mule_leave_frame()")
        self.line(f"br label %{join}")
        self.lines.append(fallback + ":")
        general = self.operation(fallback_call)
        for value in (receiver, *arguments):
            self.release(value)
        self.line(f"br label %{join}")
        self.lines.append(join + ":")
        return self.operation(
            f"phi ptr [null, %{failed}], [{result}, %{entered}], [{general}, %{fallback}]"
        )

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
        fallback_call = (
            self.keyword_expression(callee, "null", arguments, keywords)
            if keywords
            else f"call ptr @mule_call(ptr {callee}, i64 {len(arguments)}, ptr %argv)"
        )
        if function is None:
            result = self.operation(fallback_call)
            for value in arguments:
                self.release(value)
            return result
        order = function.call_order(keywords, len(arguments))
        supplied = arguments
        arguments = [arguments[index] if index >= 0 else None for index in order]
        argument_types = tuple(
            (argument_refs[index].tn.type or UNKNOWN) if index >= 0 else None
            for index in order
        )
        join = self.fresh("call.join")[1:]
        direct = self.fresh("call.direct")[1:]
        fallback_label = self.fresh("call.fallback")[1:]
        matched = self.operation(
            f"call i32 @mule_function_matches(ptr {callee}, "
            f"ptr {module.functions[function]}, ptr %globals)"
        )
        matching = self.operation(f"icmp ne i32 {matched}, 0")
        self.line(f"br i1 {matching}, label %{direct}, label %{fallback_label}")
        self.lines.append(direct + ":")
        incoming = []

        def invoke(context) -> None:
            entered = self.fresh("call.entered")[1:]
            failed = self.fresh("call.failed")[1:]
            frame = self.operation("call i32 @mule_enter_frame()")
            rejected = self.operation(f"icmp slt i32 {frame}, 0")
            self.line(f"br i1 {rejected}, label %{failed}, label %{entered}")
            self.lines.append(failed + ":")
            for argument in supplied:
                self.release(argument)
            self.line(f"br label %{join}")
            incoming.append(("null", failed))
            self.lines.append(entered + ":")
            owned = []
            for index, argument in zip(order, arguments, strict=True):
                if argument is None:
                    borrowed = self.operation(
                        f"call ptr @mule_function_default(ptr {callee}, i32 {-2 - index})"
                    )
                    argument = self.operation(
                        f"call ptr @mule.fast.retain(ptr {borrowed})"
                    )
                owned.append(argument)
            operands = ", ".join(
                [
                    "ptr %globals",
                    *module.closure_operand(self, function, callee),
                    *(f"ptr {value}" for value in owned),
                ]
            )
            body = (
                module.specialized[function, context.id]
                if context
                else module.bodies[function]
            )
            result = self.operation(f"call ptr {body}({operands})")
            self.line("call void @mule_leave_frame()")
            if context:
                # The callable identity and arguments were checked on this arm.
                # NULL remains an error result; the generic fallback has no fact.
                self.refine_type(context.return_type, result, nullable=True)
            self.line(f"br label %{join}")
            incoming.append((result, entered))

        contexts = sorted(
            module.variants_of(function),
            key=lambda context: sum(
                fact.maximum - fact.minimum if fact.is_fixnum else 2**64
                for fact in context.parameters
            ),
        )
        # Guarded entries take every argument; a defaulted one rules them out.
        supplied_arguments = [
            (source, value)
            for source, value in zip(argument_types, arguments, strict=True)
            if source is not None and value is not None
        ]
        if len(supplied_arguments) != len(order):
            contexts = []
        else:
            contexts = [
                context
                for context in contexts
                if all(
                    source.may_dispatch_fit(expected)
                    for (source, _), expected in zip(
                        supplied_arguments, context.parameters, strict=True
                    )
                )
            ]
        for context in contexts:
            guards = [
                self.argument_guard(value, expected)
                for (source, value), expected in zip(
                    supplied_arguments, context.parameters, strict=True
                )
                if not source.dispatch_fits(expected)
            ]
            if not guards:
                invoke(context)
                break
            valid = guards[0]
            for guard in guards[1:]:
                valid = self.operation(f"and i1 {valid}, {guard}")
            typed = self.fresh("call.typed")[1:]
            next_ = self.fresh("call.general")[1:]
            self.line(f"br i1 {valid}, label %{typed}, label %{next_}")
            self.lines.append(typed + ":")
            invoke(context)
            self.lines.append(next_ + ":")
        else:
            invoke(None)
        self.lines.append(fallback_label + ":")
        fallback = self.operation(fallback_call)
        for argument in supplied:
            self.release(argument)
        self.line(f"br label %{join}")
        incoming.append((fallback, fallback_label))
        self.lines.append(join + ":")
        alternatives = ", ".join(f"[ {value}, %{label} ]" for value, label in incoming)
        return self.operation(f"phi ptr {alternatives}")

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
        for tn in [*self.local_tns, *self.value_tns]:
            self.line(f"{tn.slot} = alloca {tn.sc.llvm_type}")
            self.line(f"store {tn.sc.llvm_type} {tn.sc.zero}, ptr {tn.slot}")
        for tn in self.range_tns:
            self.line(f"{tn.slot} = alloca [3 x i64]")
            self.line(f"store [3 x i64] zeroinitializer, ptr {tn.slot}")
        if self.range_tns:
            self.line("%range.buffer = alloca [3 x i64]")
        self.line(f"%argv = alloca [{max(1, self.max_args)} x ptr]")
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
                handler_live = _value_tns_live_in(self.component, set(self.value_tns))
            label = self.component.ir2_blocks[target].label
            self.lines.append("unwind." + label + ":")
            # Values the handler still uses were computed before its region
            # was entered, like the stack below an SBCL catch block; only the
            # temporaries the transfer abandons are released.
            live = handler_live[self.component.ir2_blocks[target]]
            for tn in self.value_tns:
                if tn.sc not in OBJECT or tn in live:
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
        for tn in [*self.local_tns, *self.value_tns]:
            if tn.sc is DESCRIPTOR:
                fact = tn.type or UNKNOWN
                if fact.is_fixnum or fact.is_immortal:
                    continue
                loaded = self.operation(f"load ptr, ptr {tn.slot}")
                if len(self.component.blocks) > 8:
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
        for tn in [*self.local_tns, *self.value_tns]:
            self.line(f"{tn.slot} = alloca {tn.sc.llvm_type}")
            if tn.sc in OBJECT:
                self.line(f"store ptr null, ptr {tn.slot}")
        for slot in self.owner_slots.values():
            self.line(f"{slot} = alloca ptr")
            self.line(f"store ptr null, ptr {slot}")
        self.line(f"%argv = alloca [{max(1, self.max_args)} x ptr]")
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
        fast = (
            ast.ParameterKind.KEYWORD_ONLY not in kinds
            and ast.ParameterKind.VAR_KEYWORD not in kinds
        )
        lines = [
            f"define ptr {module.functions[lambda_]}(ptr %globals, ptr %args) {{",
            "entry:",
        ]
        for index in range(count):
            lines += [
                f"  %slot.{index} = alloca ptr",
                f"  store ptr null, ptr %slot.{index}",
            ]
        if fast:
            lines += [
                f"  %plain.status = call i32 @mule_binding_plain(ptr %args, i32 {positional}, i32 {int(lambda_.signature.variadic)})",
                "  %fits = icmp ne i32 %plain.status, 0",
                "  br i1 %fits, label %plain, label %binding",
                "plain:",
            ]
            for index in range(count):
                getter = (
                    "mule_rest_args"
                    if kinds[index] == ast.ParameterKind.VAR_POSITIONAL
                    else "mule_arg"
                )
                lines += [
                    f"  %plain.{index} = call ptr @{getter}(ptr %args, i32 {index})",
                    f"  store ptr %plain.{index}, ptr %slot.{index}",
                    f"  %plain.missing.{index} = icmp eq ptr %plain.{index}, null",
                    f"  br i1 %plain.missing.{index}, label %failed, label %plain.ok.{index}",
                    f"plain.ok.{index}:",
                ]
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
            f"  %signature = call ptr {module.constant_tuple(lambda_.binding)}()",
            f"  %bound = call ptr {binder}(ptr %private, ptr %owned.view, ptr %signature)",
            "  %unbound = icmp eq ptr %bound, null",
            "  br i1 %unbound, label %failed, label %bound.ok",
            "bound.ok:",
        ]
        for index in range(count):
            lines += [
                f"  %bound.{index} = call ptr @mule_arg(ptr %bound, i32 {index})",
                f"  store ptr %bound.{index}, ptr %slot.{index}",
            ]
        lines += [
            "  call void @mule.fast.release(ptr %bound)",
            "  br label %invoke",
            "invoke:",
        ]
        for index in range(count):
            lines.append(f"  %parameter.{index} = load ptr, ptr %slot.{index}")
        arguments = ", ".join(
            [
                "ptr %globals",
                *self.wrapper_closure(lines),
                *(f"ptr %parameter.{index}" for index in range(count)),
            ]
        )
        lines += [
            f"  %result = call ptr {module.bodies[lambda_]}({arguments})",
            "  ret ptr %result",
            "failed:",
        ]
        for index in range(count):
            lines += [
                f"  %old.{index} = load ptr, ptr %slot.{index}",
                f"  call void @mule.fast.release(ptr %old.{index})",
            ]
        lines += ["  ret ptr null", "}"]
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
        ]
        for index in range(count):
            lines.extend(
                [
                    f"  %slot.{index} = alloca ptr",
                    f"  store ptr null, ptr %slot.{index}",
                ]
            )
        lines.extend(
            [
                f"  %status = call i32 @{check}(ptr %args, i32 {fixed})",
                "  %wrong = icmp slt i32 %status, 0",
                "  br i1 %wrong, label %failed, label %checked",
                "checked:",
            ]
        )
        for index in range(count):
            getter = "mule_rest_args" if variadic and index == fixed else "mule_arg"
            lines.extend(
                [
                    f"  %arg.{index} = call ptr @{getter}(ptr %args, i32 {index})",
                    f"  store ptr %arg.{index}, ptr %slot.{index}",
                    f"  %missing.{index} = icmp eq ptr %arg.{index}, null",
                    f"  br i1 %missing.{index}, label %failed, label %arg.ok.{index}",
                    f"arg.ok.{index}:",
                ]
            )
        arguments = ", ".join(
            [
                "ptr %globals",
                *self.wrapper_closure(lines),
                *(f"ptr %arg.{index}" for index in range(count)),
            ]
        )
        lines.extend(
            [
                f"  %result = call ptr {module.bodies[lambda_]}({arguments})",
                "  ret ptr %result",
                "failed:",
            ]
        )
        for index in range(count):
            lines.extend(
                [
                    f"  %old.{index} = load ptr, ptr %slot.{index}",
                    f"  call void @mule.fast.release(ptr %old.{index})",
                ]
            )
        lines.extend(["  ret ptr null", "}"])
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
