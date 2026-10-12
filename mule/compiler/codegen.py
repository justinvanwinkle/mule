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

from mule import ast
from mule.diagnostics import phase

from . import vops
from .ctype import UNKNOWN, ValueType
from .ir1util import block_exception_target, class_method_definitions
from .ir2tran import ir2_convert
from .ltn import EntryContext, ltn_analyze
from .node import CallKind, Combination, FunctionalKind, Lambda, LambdaVar, Signature
from .represent import select_native_plans, select_representations
from .vop import BOOLEAN, CLASS, DESCRIPTOR, DOUBLE, IMMEDIATE, NULL, RANGE, SIGNED

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
declare ptr @mule_namespace()
declare i32 @mule_namespace_bound(ptr, ptr)
declare i32 @mule_assertion_failure(ptr)
declare ptr @mule_sequence_list(i64, ptr)
declare ptr @mule_sequence_tuple(i64, ptr)
declare ptr @mule_sequence_tuple_owned(i64, ptr)
declare ptr @mule_lookup_special(ptr, ptr)
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
declare ptr @mule_arg(ptr, i32)
declare ptr @mule_retain(ptr)
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
declare i32 @mule_clear_binding(ptr, ptr)
declare i32 @mule_reraise()
declare i32 @mule_check_rest_args(ptr, i32)
declare ptr @mule_rest_args(ptr, i32)
declare ptr @mule_storage_length(ptr)
declare ptr @mule_storage_read(ptr, ptr)
declare ptr @mule_storage_write(ptr, ptr, ptr)
declare ptr @mule_storage_append(ptr, ptr)
declare ptr @mule_storage_insert(ptr, ptr, ptr)
declare ptr @mule_storage_delete(ptr, ptr, ptr)
declare ptr @mule_storage_tuple(ptr)
declare ptr @mule_tuple_storage(ptr)
declare i32 @mule_tuple_shape_matches(ptr, i64)
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
declare ptr @mule_builtins_module()
declare ptr @mule_import_missing(ptr)
declare void @mule_module_loaded(ptr, ptr)
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
    closure: bool

    def call_order(self, keywords, count, *, receiver=False):
        return self.signature.call_order(
            self.parameters, keywords, count, receiver=receiver
        )


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
    print_entry: Lambda | None = None
    protocol_entry: Lambda | None = None
    sequence_functions: dict[str, Lambda] = field(default_factory=dict)
    argument_constructor: Lambda | None = None
    argument_binder: Lambda | None = None
    layouts: dict[str, object] = field(default_factory=dict)
    external: list[Lambda] = field(default_factory=list)
    external_facts: dict = field(default_factory=dict)
    unit: str = "program"
    exports: bool = False
    # Units the main unit initializes before its own, in order.
    dependencies: list[str] = field(default_factory=list)
    # Other units' method definitions, predictions after this unit's own.
    external_methods: dict[str, object] = field(default_factory=dict)
    # A module unit's own import: its initializer belongs to this unit and
    # initializes the unit before running the module body.
    module_import: object = None


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

    def __getitem__(self, key):
        lambda_, variant = key if isinstance(key, tuple) else (key, None)
        if variant is None:
            name = f"{lambda_.name}.{self.suffix}"
        else:
            name = f"{lambda_.name}.spec.{self.module.variant_index(lambda_, variant)}"
        if lambda_ not in self.module.owned:
            self.module.foreign[name] = (self.suffix, lambda_)
        return _symbol(name)


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
        self.foreign: dict[str, tuple[str, Lambda]] = {}
        self.functions = _Symbols(self, "fn")
        self.bodies = _Symbols(self, "body")
        self.specialized = _Symbols(self, "body")
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
        self.auxiliary: list[str] = []
        self.method_targets: dict[str, Lambda] = {}
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
        for block in lambda_.component.blocks():
            for node in block.nodes():
                if (
                    isinstance(node, Combination)
                    and node.kind is CallKind.KNOWN
                    and node.fun_info.name == "%import"
                ):
                    self.imports.setdefault(node.info.module_name, node.info)

    def variants_of(self, lambda_: Lambda):
        facts = self.facts.get(lambda_)
        return facts.variants if facts is not None else ()

    def closure_of(self, lambda_) -> bool:
        """Whether the function body takes a closure parameter."""
        if isinstance(lambda_, ForeignFunction):
            return lambda_.closure
        environment = lambda_.environment
        return environment is not None and bool(environment.closure)

    def closure_operand(self, gen, lambda_: Lambda, callable_value: str) -> list[str]:
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

    def constant_tuple(self, literal: ast.Tuple) -> str:
        """Register dependencies before their parent, sharing only typed values."""
        key = literal.accept(_ConstantKey())
        existing = self.tuple_constants.get(key)
        if existing is not None:
            return existing.entry
        for element in literal.elements:
            if isinstance(element, ast.Tuple):
                self.constant_tuple(element)
        index = len(self.tuple_constants)
        constant = TupleConstant(
            literal, f"@mule.literal.slot.{index}", f"@mule.literal.tuple.{index}"
        )
        self.tuple_constants[key] = constant
        self.auxiliary.extend(
            [
                f"{constant.slot} = internal global ptr null",
                (
                    f"define internal nonnull ptr {constant.entry}() alwaysinline {{\n"
                    "entry:\n"
                    f"  %value = load ptr, ptr {constant.slot}\n"
                    "  %owned = call ptr @mule.fast.retain(ptr %value)\n"
                    "  ret ptr %owned\n}"
                ),
            ]
        )
        return constant.entry

    def initialize_tuple_constants(self) -> str:
        """Construct the pool inside the runtime's startup error boundary.

        The argument buffer owns each scalar/child reference until construction
        finishes. On failure it is unwound here; main releases initialized roots.
        """
        if not self.tuple_constants:
            return ""
        count = max(
            1,
            max(
                len(constant.literal.elements)
                for constant in self.tuple_constants.values()
            ),
        )
        lines = [
            f"  %literal.argv = alloca [{count} x ptr]",
            f"  store [{count} x ptr] zeroinitializer, ptr %literal.argv",
        ]
        for index, constant in enumerate(self.tuple_constants.values()):
            operands = []
            for position, element in enumerate(constant.literal.elements):
                value = f"%literal.{index}.value.{position}"
                address = f"%literal.{index}.arg.{position}"
                operation = element.accept(vops.LiteralEmitter(self))
                lines.extend(
                    [
                        f"  {value} = {operation}",
                        f"  {address} = getelementptr [{count} x ptr], ptr %literal.argv, i64 0, i64 {position}",
                        f"  store ptr {value}, ptr {address}",
                    ]
                )
                operands.append((value, address))
                if operation.startswith("call "):
                    lines.extend(
                        [
                            f"  %literal.{index}.bad.{position} = icmp eq ptr {value}, null",
                            f"  br i1 %literal.{index}.bad.{position}, label %literal.failure, label %literal.{index}.after.{position}",
                            f"literal.{index}.after.{position}:",
                        ]
                    )
            result = f"%literal.{index}.tuple"
            lines.extend(
                [
                    f"  {result} = call ptr @mule_sequence_tuple_owned(i64 {len(operands)}, ptr %literal.argv)",
                    f"  store ptr {result}, ptr {constant.slot}",
                ]
            )
            for _, address in operands:
                lines.append(f"  store ptr null, ptr {address}")
            lines.extend(
                [
                    f"  %literal.{index}.bad = icmp eq ptr {result}, null",
                    f"  br i1 %literal.{index}.bad, label %literal.failure, label %literal.{index}.ready",
                    f"literal.{index}.ready:",
                ]
            )
        lines.extend(["  br label %protocols", "literal.failure:"])
        for position in range(count):
            address = f"%literal.failed.arg.{position}"
            value = f"%literal.failed.value.{position}"
            lines.extend(
                [
                    f"  {address} = getelementptr [{count} x ptr], ptr %literal.argv, i64 0, i64 {position}",
                    f"  {value} = load ptr, ptr {address}",
                    f"  call void @mule.fast.release(ptr {value})",
                ]
            )
        lines.extend(["  ret ptr null", "protocols:"])
        return "\n".join(lines) + "\n"

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
        for lambda_ in program.lambdas:
            general, variants = self.contexts(lambda_)
            functions.append(self.emit_body(lambda_, general))
            functions.extend(self.emit_body(lambda_, variant) for variant in variants)
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
        self.auxiliary.extend(
            f"{layout.schema} = internal global i8 0"
            for layout in self.layouts.values()
        )
        self.auxiliary.append(self.unit_lifecycle(owned_imports))
        if program.entry is not None:
            self.auxiliary.extend(self.executable_entry())
        # Data first: the bodies emitted above registered their constants.
        globals_ = []
        for value, name in self.data.items():
            escaped = "".join(f"\\{byte:02X}" for byte in value + b"\0")
            globals_.append(
                f"{name} = private unnamed_addr constant "
                f'[{len(value) + 1} x i8] c"{escaped}"'
            )
        return (
            "\n\n".join(
                [
                    DECLARATIONS,
                    self.foreign_declarations(),
                    vops.helpers(),
                    *globals_,
                    *self.auxiliary,
                    *functions,
                ]
            )
            + "\n"
        )

    def foreign_declarations(self) -> str:
        lines = []
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
        for info in self.imports.values():
            if not self.owns_import(info):
                lines.append(f"declare ptr {self.import_symbol(info)}()")
        return "\n".join(lines)

    def unit_lifecycle(self, owned_imports) -> str:
        """The unit's initializer, which builds its tuple constants, and its
        finalizer, which releases them and the modules it owns."""
        linkage = "" if self.program.exports else "internal "
        initialize = (
            f"define {linkage}ptr {unit_initializer(self.program.unit)}() {{\n"
            "entry:\n"
            + self.initialize_tuple_constants()
            + "  ret ptr inttoptr (i64 1 to ptr)\n}"
        )
        releases = [
            (f"%import.{index}", self.import_cache(info))
            for index, info in enumerate(owned_imports)
            if not info.missing
        ] + [
            (f"%literal.root.{index}", constant.slot)
            for index, constant in reversed(
                list(enumerate(self.tuple_constants.values()))
            )
        ]
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
        """``main`` and the protocol startup it runs: every unit's constants,
        then the sequence and print protocols."""
        program = self.program
        units = [*program.dependencies, program.unit]
        definitions = [
            f"declare ptr {unit_initializer(unit)}()\ndeclare void {unit_finalizer(unit)}()"
            for unit in program.dependencies
        ]
        startup = []
        for index, unit in enumerate(units):
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
            + f"  %sequence = call ptr {self.functions[program.protocol_entry]}(ptr %globals, ptr %args)\n"
            "  %failed = icmp eq ptr %sequence, null\n"
            "  call void @mule_release(ptr %sequence)\n"
            "  br i1 %failed, label %failure, label %print\n"
            "print:\n"
            f"  %result = call ptr {self.functions[program.print_entry]}(ptr %globals, ptr %args)\n"
            "  ret ptr %result\n"
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
            f"call i32 @mule_run_protocols(ptr {self.functions[program.entry]}, "
            "ptr @mule.initialize.protocols)"
        )
        definitions.append(
            "define i32 @main(i32 %argc, ptr %argv) {\n"
            "entry:\n"
            "  call void @mule_set_process_arguments(i32 %argc, ptr %argv)\n"
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

    def import_initializer(self, info):
        symbol = self.import_symbol(info)
        if info.missing:
            # Without compiled source, a module can still be one the program
            # installed in sys.modules; otherwise this raises ModuleNotFoundError.
            return "\n".join(
                [
                    f"define internal ptr {symbol}() {{",
                    "entry:",
                    f"  %module = call ptr @mule_import_missing(ptr {self.text(info.module_name)})",
                    "  ret ptr %module",
                    "}",
                ]
            )
        # Every unit importing a native module generates the same initializer;
        # the linker keeps one, so the module stays a singleton. A compiled
        # module's initializer is defined only by the unit owning its body.
        linkage = "linkonce_odr " if info.native else ""
        cache = self.import_cache(info)
        self.auxiliary.append(f"{cache} = {linkage or 'internal '}global ptr null")
        lines = [
            f"define {linkage}ptr {symbol}() {{",
            "entry:",
            f"  %cached = load ptr, ptr {cache}",
            "  %ready = icmp ne ptr %cached, null",
            "  br i1 %ready, label %existing, label %initialize",
            "existing:",
            "  %owned = call ptr @mule_retain(ptr %cached)",
            "  ret ptr %owned",
            "initialize:",
            *self.unit_startup(info),
            # The builtins module is the namespace builtin lookups consult.
            "  %globals = call ptr @mule_builtins_module()"
            if info.module_name == "builtins"
            else "  %globals = call ptr @mule_namespace()",
            f"  store ptr %globals, ptr {cache}",
        ]
        fields = [("__name__", info.module_name)]
        if info.filename is not None:
            fields.append(("__file__", info.filename))
        # The module is in sys.modules while its body runs, as in CPython.
        lines.append(
            f"  call void @mule_module_loaded(ptr {self.text(info.module_name)}, ptr %globals)"
        )
        for field_name, value in fields:
            literal = self.literal_data(value.encode())
            key = self.text(field_name)
            label = field_name.strip("_")
            lines.extend(
                [
                    f"  %{label} = call ptr @mule_literal_text(ptr {literal}, i64 {len(value.encode())})",
                    f"  %status.{label} = call i32 @mule_store_global(ptr %globals, ptr {key}, ptr %{label})",
                    f"  call void @mule_release(ptr %{label})",
                    f"  %bad.{label} = icmp slt i32 %status.{label}, 0",
                    f"  br i1 %bad.{label}, label %failed, label %after.{label}",
                    f"after.{label}:",
                ]
            )
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
                f"  store ptr null, ptr {cache}",
                "  call void @mule_release(ptr %globals)",
                "  ret ptr null",
                "}",
            ]
        )
        return "\n".join(lines)


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


class _ArgumentGuard:
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
        tagged = operation(f"icmp ne i64 {tag}, 0")
        decoded = operation(f"ashr i64 {bits}, 1")
        lower = operation(f"icmp sge i64 {decoded}, {fact.minimum}")
        upper = operation(f"icmp sle i64 {decoded}, {fact.maximum}")
        bounds = operation(f"and i1 {lower}, {upper}")
        return operation(f"and i1 {tagged}, {bounds}")

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
            elif isinstance(tn.leaf, LambdaVar) and tn.leaf.expression_temporary:
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
        self.landings: dict[tuple[str, tuple[str, ...]], str] = {}
        self.entry_end = 0
        self.uses_out = False
        self.owner_slots: dict[object, str] = {}
        if self.raw:
            self.owner_slots = {
                node: f"%owner.{index}"
                for index, node in enumerate(self.plan.allocations)
            }
            self.dependencies = {
                name: f"%class.{index}"
                for index, name in enumerate(self.plan.dependencies)
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

    def failure_target(self, base: str | None = None, failed_value=None) -> str:
        """The label a failure edge branches to: ``base`` (the handler's unwind
        block or ``error`` by default) after releasing the owned temporaries
        live here. ``failed_value`` is the NULL result being checked."""
        if base is None:
            base = (
                "error"
                if self.exception_target is None
                else "unwind." + self.exception_target
            )
        # Native entries leave value ownership to their boundary adapter.
        owned = (
            ()
            if self.raw
            else tuple(value for value in self.live.values() if value != failed_value)
        )
        if not owned:
            return base
        key = (base, owned)
        label = self.landings.get(key)
        if label is None:
            label = self.landings[key] = self.fresh("release")[1:]
        return label

    def emit_landings(self) -> None:
        for (base, owned), label in self.landings.items():
            self.lines.append(label + ":")
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

    def propagate(self) -> None:
        self.line(f"br label %{self.failure_target()}")

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

    def store(self, ref, value: str) -> None:
        tn = ref.tn
        if tn in self.registers:
            self.define(tn, value)
            return
        if tn.sc in OBJECT:
            self.publish(ref, value)
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

    def publish(self, ref, value: str) -> None:
        tn = ref.tn
        if tn in self.registers:
            self.define(tn, value)
            return
        old = self.operation(f"load ptr, ptr {tn.slot}")
        self.line(f"store ptr {value}, ptr {tn.slot}")
        self.mark_immortal(tn, old)
        self.mark_immortal(tn, value)
        if tn.sc.owned:
            self.release(old)

    def release(self, value: str) -> None:
        if value in self.immortal_values:
            return
        self.line(f"call void @mule.fast.release(ptr {value})")

    def refine(self, tn, value: str) -> None:
        """Expose proven representation/range facts after the error check."""
        self.mark_immortal(tn, value)
        self.refine_type(tn.type or UNKNOWN, value)

    def refine_type(self, fact: ValueType, value: str, *, nullable=False) -> None:
        if fact.is_fixnum:
            bits = self.operation(f"ptrtoint ptr {value} to i64")
            tagged = None
            if self.context.id is None:
                tag = self.operation(f"and i64 {bits}, 1")
                tagged = self.operation(f"icmp ne i64 {tag}, 0")
            decoded = self.operation(f"ashr i64 {bits}, 1")
            lower = self.operation(f"icmp sge i64 {decoded}, {fact.minimum}")
            upper = self.operation(f"icmp sle i64 {decoded}, {fact.maximum}")
            bounded = self.operation(f"and i1 {lower}, {upper}")
            valid = (
                self.operation(f"and i1 {tagged}, {bounded}")
                if tagged is not None
                else bounded
            )
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
        self.publish(ref, value)
        for operand in consumed:
            self.release(operand)
        self.refine(ref.tn, value)

    def parameter(self, index: int) -> str:
        return f"%parameter.{index}"

    def local_slot(self, name: str) -> str:
        return self.local_slots[name]

    def dependency(self, name: str) -> str:
        return self.dependencies[name]

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
        if (tn.type or UNKNOWN).is_fixnum:
            bits = self.operation(f"ptrtoint ptr {value} to i64")
            nonzero = self.operation(f"icmp ne i64 {bits}, 1")
            truth = self.operation(f"zext i1 {nonzero} to i32")
        else:
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
            wide = self.operation(f"zext i1 {value} to i32")
            return self.operation(f"call ptr @mule_bool(i32 {wide})")
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

    def float_arithmetic(self, opcode: str, left: str, right: str) -> str:
        if opcode == "fdiv" and self.lambda_.policy.safety:
            zero = self.operation(f"fcmp oeq double {right}, 0.0")
            failed, continuation = self.fresh("zero")[1:], self.fresh("nonzero")[1:]
            self.line(f"br i1 {zero}, label %{failed}, label %{continuation}")
            self.lines.append(failed + ":")
            self.line("call i32 @mule_native_zero_division()")
            self.line(f"br label %{self.failure_target('error')}")
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

    def compiled_call(self, function, values, globals_value="%globals") -> str:
        """Transfer owned arguments to a compiled body inside a runtime frame.

        The body owns its parameters on every return. Failed frame entry leaves
        them here, so that arm consumes them before returning the error result.
        """
        body = (
            self.module.bodies_by_name(function)
            if isinstance(function, str)
            else self.module.bodies[function]
        )
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
        globals_value = self.operation("call ptr @mule_protocol_globals()")
        return self.compiled_call(
            self.module.sequence_functions[name], values, globals_value
        )

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
        if any(index < 0 for index in order):
            contexts = []
        for context in contexts:
            guards = [
                self.argument_guard(value, expected)
                for source, value, expected in zip(
                    argument_types, arguments, context.parameters, strict=True
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
        # Other units call an exporting unit's bodies; native raw entries
        # are called only through the unit's own adapters.
        linkage = "" if self.module.program.exports and not self.raw else "internal "
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
        if lambda_.policy.speed == 0:
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
        self.line(f"%argv = alloca [{max(1, self.max_args)} x ptr]")
        self.entry_end = len(self.lines)
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
                self.release(self.operation(f"load ptr, ptr {tn.slot}"))
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
        if self.uses_out:
            self.lines.insert(self.entry_end, "  %out = alloca ptr")

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
        binder = module.bodies[module.program.argument_binder]
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
    for index, (name, prediction) in enumerate(plan.dependencies.items()):
        value = gen.operation(
            f"call ptr @mule_load_global(ptr %globals, ptr {gen.module.text(name)})"
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
