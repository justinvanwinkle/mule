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
from dataclasses import replace

from mule import ast
from mule.diagnostics import phase

from . import fndb, vops
from .callconv import (
    NumericEntry,
    entry_steps,
    numeric_entry,
    select_numeric_calls,
    tuple_return_convention,
)
from .codegen_support import (
    _BORROWED_SEQUENCE_PROTOCOLS,
    _CONSTANT_KEY,
    _DECLARATION_LINES,
    _HEX_BYTE,
    _MULE_SYMBOL_RE,
    DECLARATIONS,
    PROTOCOL_NAMESPACE,
    ForeignFunction,
    Function,
    Program,
    TupleConstant,
    _ArgumentGuard,
    _CallTargetCollector,
    _ConstantKey,
    _declared_protocol_context,
    _function_comment,
    _helper_comment,
    _HelperBuilder,
    _ModuleCollector,
    _needs_cleanup_release,
    _NestedTuples,
    _parsed_helpers,
    _SpecializedSymbols,
    _suffixed,
    _symbol,
    _Symbols,
    _TypeAssumption,
    _UnpackedSymbols,
    _without_llvm_comments,
    compile_component,
    logger,
    runtime_namespace,
    unit_finalizer,
    unit_initializer,
)
from .ctype import (
    UNKNOWN,
    ValueType,
)
from .escape import analyze_escapes, find_local_environments
from .function_codegen import FunctionGenerator
from .ir2tran import select_tuple_delivery
from .locall import specialized_ir1_context
from .ltn import EntryContext, FixnumGuard, primitive_type_of
from .node import (
    CodeInfo,
    FunctionalKind,
    Lambda,
    linked,
)
from .represent import (
    fresh_box_returns,
    mark_private_boxes,
    select_native_plans,
)
from .traceback_sites import SourceHighlights
from .vop import (
    CLASS,
    DESCRIPTOR,
    DOUBLE,
    FLOAT_BOX_LAYOUT,
    IMMEDIATE,
    SIGNED,
    BoxLayout,
)


def generate(program: Program) -> str:
    """Return a complete LLVM module with a native executable entry point."""
    with phase("LLVM generation %s", program.unit or "__main__"):
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
            self.native_plans = {
                function: plan
                for function, plan in select_native_plans(
                    lambdas, self.facts, self.layouts
                ).items()
                if function.entry_parameter_types is None
            }
        self.return_tuple_arities: dict[Lambda, int] = {}
        self.return_tuple_uses: dict[Lambda, frozenset] = {}
        for lambda_ in lambdas:
            if (
                lambda_ not in self.native_plans
                and lambda_.entry_parameter_types is None
            ):
                info = tuple_return_convention(lambda_)
                if info is not None:
                    (
                        self.return_tuple_arities[lambda_],
                        self.return_tuple_uses[lambda_],
                    ) = info.arity, info.direct
        self.local_environments = find_local_environments(lambdas)
        self.escape_facts = analyze_escapes(
            lambdas, local_environments=self.local_environments, expand=False
        )
        escapes = self.escape_facts
        self.borrowed_callees = {}
        for function in lambdas:
            if (
                function is program.entry
                or function in self.native_plans
                or function in self.return_tuple_arities
                or (
                    function.takes_closure
                    and not self.local_environments.is_local(function)
                )
                or function.signature.variadic
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
                and (
                    function.is_protocol
                    or not escapes.parameter_escapes(function, index)
                )
            )
            if candidates:
                self.borrowed_callees[function] = candidates
            fn_facts = self.facts.get(function)
            if (
                not function.is_protocol
                and fn_facts is not None
                and fn_facts.variants
                and len(candidates) < len(function.vars)
            ):
                variant_borrows = []
                for variant in fn_facts.variants:
                    variant_escapes = analyze_escapes(
                        [function],
                        local_environments=self.local_environments,
                        base_facts=escapes,
                        expand=False,
                        node_types=variant.types,
                    )
                    variant_candidates = frozenset(
                        index
                        for index, var in enumerate(function.vars)
                        if not var.indirect
                        and not var.sets
                        and not variant_escapes.parameter_escapes(function, index)
                    )
                    if variant_candidates - candidates:
                        variant_borrows.append((variant.parameters, variant_candidates))
                if variant_borrows:
                    self.borrowed_callees[function, "variants"] = tuple(variant_borrows)
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
        # Namespaces whose cells a weak_odr initializer uses, which must
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
        call_edges: dict[Lambda, set[Lambda]] = {l: set() for l in lambdas}
        may_call_external: set[Lambda] = set()
        for l in lambdas:
            call_collector = _CallTargetCollector(self.owned, self.method_targets)
            for block in linked(l.component, l, "component").blocks():
                for node in block.nodes():
                    node.accept(call_collector)
            call_edges[l] = call_collector.callees
            if call_collector.may_call_unknown:
                may_call_external.add(l)
        self.may_call_external: set[Lambda] = set()
        self.may_recurse: set[Lambda] = set()
        for start in lambdas:
            if start in may_call_external:
                self.may_call_external.add(start)
                self.may_recurse.add(start)
            visited: set[Lambda] = set()
            stack = list(call_edges[start])
            while stack:
                cur = stack.pop()
                if cur is start:
                    self.may_recurse.add(start)
                if cur in may_call_external:
                    self.may_call_external.add(start)
                    self.may_recurse.add(start)
                if cur not in visited:
                    visited.add(cur)
                    stack.extend(call_edges[cur])

    def needs_frame_depth_check(self, function: Function) -> bool:
        if function not in self.owned:
            return True
        if function.policy.debug >= 2:
            return True
        if function.policy.safety == 0:
            return False
        return function in self.may_recurse

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

    def variants_of(self, lambda_: Function):
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
        """The function returning the imported module. A missing or unowned
        module's is private to the unit; an owned compiled module's belongs
        to its own unit."""
        if info.missing or not self.owns_import(info):
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

    def store_global(
        self, namespace: str, name: str, value: str, *, final_binding: bool = False
    ) -> str:
        """The call binding global ``name`` to ``value``, borrowed; its status
        is negative on failure."""
        self.bound_namespaces.add(runtime_namespace(namespace))
        cell = self.cell(namespace, name)
        return (
            f"call i32 @mule_store_global(ptr {cell}, ptr {self.text(name)}, "
            f"ptr {value}, i32 {int(final_binding)})"
        )

    def delete_global(self, var, *, missing_ok: bool = False) -> str:
        self.bound_namespaces.add(runtime_namespace(var.namespace))
        cell = self.cell(var.namespace, var.name)
        if missing_ok:
            return (
                f"call i32 @mule_unbind_global(ptr {cell}, ptr {self.text(var.name)})"
            )
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
            linkage = "weak_odr" if shared else "internal"
            constant = "weak_odr constant" if shared else "private constant"
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
                _helper_comment(f"global linkage for namespace {namespace}")
                + "\n"
                + f"define {linkage} i32 {_symbol(f'mule.link:{namespace}')}(ptr %namespace) {{\n"
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
        if lambda_.generator_outer is not None:
            return self.code_descriptor(lambda_.generator_outer)
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
        if lambda_.generator_outer is not None:
            return self.code_metadata(lambda_.generator_outer)
        code = self.code(lambda_)
        signature = lambda_.signature
        parameters = lambda_.parameters
        kinds = signature.kinds
        body = lambda_.generator_body or lambda_
        closed = self.closed_variables.get(body, set())
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
            for var in body.locals
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
            self.auxiliary.append(f"{name} = weak_odr global i64 0")
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
            _helper_comment(f"cached tuple packing; arity: {count}")
            + "\n"
            + f"define internal ptr {symbol}({params}) {{\n"
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
            _helper_comment(
                f"tuple constant initialization; unit: {self.program.unit}"
            ),
            f"define internal ptr {symbol}() {{",
            "entry:",
            f"  %cached = load ptr, ptr {slot}",
            "  %present = icmp ne ptr %cached, null",
            f"  br i1 %present, label %ready, label %build, {vops.HOT_BRANCH}",
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
                f"  %built = call ptr {packer}({', '.join(operands)}) cold noinline",
                "  ret ptr %built",
                "ready:",
                "  %owned = call ptr @mule_retain(ptr %cached)",
                "  ret ptr %owned",
                "}",
            ]
        )
        return "\n".join(lines)

    def protocol_body(self, name: str) -> str:
        """An entry valid for this protocol's fixed caller-proven domain.

        Public calls still use the generic body. Missing imported proof keeps
        the protocol generic too, rather than inventing a receiver layout.
        """
        function = self.sequence_functions[name]
        info = fndb.PROTOCOLS.get(name)
        if (
            info is not None
            and info.argument_types is not None
            and function.entry_parameter_types is not None
        ):
            return self.reference(function, f"{function.name}.protocol", "body")
        return self.bodies[function]

    def protocol_helper(self, name: str, arity: int) -> str:
        existing = self.protocol_helpers.get(name)
        if existing is not None:
            return existing
        symbol = _symbol(f"mule.protocol.{name}")
        self.protocol_helpers[name] = symbol
        body = self.protocol_body(name)
        params = ", ".join(f"ptr %arg.{i}" for i in range(arity))
        operands = ", ".join(["ptr %globals", *(f"ptr %arg.{i}" for i in range(arity))])
        borrowed_protocol = name in _BORROWED_SEQUENCE_PROTOCOLS
        releases = (
            ""
            if borrowed_protocol
            else "\n".join(
                f"  call void @mule.fast.release(ptr %arg.{i}) cold noinline"
                for i in range(arity)
            )
        )
        attrs = (
            " cold noinline"
            if name in ("raise_value", "iteration_exhausted")
            else " noinline"
        )
        entry_guard = (
            "  %arg.null = icmp eq ptr %arg.0, null\n"
            f"  br i1 %arg.null, label %failed.raise, label %frame.enter, {vops.COLD_BRANCH}\n"
            "frame.enter:\n"
            if name == "raise_value"
            else ""
        )
        tail = (
            (
                "  %raised.null = icmp eq ptr %result, null\n"
                f"  br i1 %raised.null, label %failed.raise, label %raise.pending, {vops.COLD_BRANCH}\n"
                "raise.pending:\n"
                "  call ptr @mule_raise_pending(ptr %result) cold\n"
                "  call void @mule.fast.release(ptr %result)\n"
                "  ret ptr @mule.none\n"
                "failed.raise:\n"
                "  ret ptr null\n"
            )
            if name == "raise_value"
            else "  ret ptr %result\n"
        )
        self.auxiliary.append(
            _helper_comment(
                f"protocol adapter: {name}; target: {self.sequence_functions[name].name}"
            )
            + "\n"
            + f"define internal ptr {symbol}({params}){attrs} {{\n"
            "entry:\n"
            + entry_guard
            + "  %globals = call ptr @mule_protocol_globals()\n"
            "  %status = call i32 @mule_enter_frame()\n"
            "  %bad = icmp slt i32 %status, 0\n"
            f"  br i1 %bad, label %failed, label %entered, {vops.COLD_BRANCH}\n"
            "failed:\n" + (releases + "\n" if releases else "") + "  ret ptr null\n"
            "entered:\n"
            f"  %result = call ptr {body}({operands})\n"
            "  call void @mule_leave_frame()\n" + tail + "}"
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
            _helper_comment("getitem protocol dispatch")
            + "\n"
            + f"define internal ptr {symbol}(ptr %sequence, ptr %index) {{\n"
            "entry:\n"
            "  %fast = call ptr @mule_fast_getitem(ptr %sequence, ptr %index)\n"
            "  %matched = icmp ne ptr %fast, null\n"
            f"  br i1 %matched, label %hit, label %slow, {vops.HOT_BRANCH}\n"
            "hit:\n"
            "  ret ptr %fast\n"
            "slow:\n"
            "  %owned.seq = call ptr @mule.fast.retain(ptr %sequence) cold noinline\n"
            "  %owned.idx = call ptr @mule.fast.retain(ptr %index) cold noinline\n"
            f"  %result = call ptr {slow}(ptr %owned.seq, ptr %owned.idx) cold noinline\n"
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
            _helper_comment("slice protocol dispatch")
            + "\n"
            + f"define internal ptr {symbol}(ptr %sequence, ptr %start, ptr %stop, ptr %step) {{\n"
            "entry:\n"
            "  %fast = call ptr @mule_fast_getslice(ptr %sequence, ptr %start, ptr %stop, ptr %step)\n"
            "  %matched = icmp ne ptr %fast, null\n"
            f"  br i1 %matched, label %hit, label %slow, {vops.HOT_BRANCH}\n"
            "hit:\n"
            "  ret ptr %fast\n"
            "slow:\n"
            "  %owned.start = call ptr @mule.fast.retain(ptr %start) cold noinline\n"
            "  %owned.stop = call ptr @mule.fast.retain(ptr %stop) cold noinline\n"
            "  %owned.step = call ptr @mule.fast.retain(ptr %step) cold noinline\n"
            f"  %slice = call ptr {make_slice}(ptr %owned.start, ptr %owned.stop, ptr %owned.step) cold noinline\n"
            "  %bad = icmp eq ptr %slice, null\n"
            f"  br i1 %bad, label %failed, label %lookup, {vops.COLD_BRANCH}\n"
            "failed:\n"
            "  ret ptr null\n"
            "lookup:\n"
            "  %owned.seq = call ptr @mule.fast.retain(ptr %sequence) cold noinline\n"
            f"  %result = call ptr {slow}(ptr %owned.seq, ptr %slice) cold noinline\n"
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
            ""
            if move
            else "  %owned.item = call ptr @mule.fast.retain(ptr %item) cold noinline\n"
        )
        item_arg = "%item" if move else "%owned.item"
        self.auxiliary.append(
            _helper_comment(f"setitem protocol dispatch; move: {move}")
            + "\n"
            + f"define internal ptr {symbol}(ptr %sequence, ptr %index, ptr %item) {{\n"
            "entry:\n"
            f"  %matched = call i32 {fast_fn}(ptr %sequence, ptr %index, ptr %item)\n"
            "  %valid = icmp ne i32 %matched, 0\n"
            f"  br i1 %valid, label %hit, label %slow, {vops.HOT_BRANCH}\n"
            "hit:\n"
            "  ret ptr @mule.none\n"
            "slow:\n"
            "  %owned.seq = call ptr @mule.fast.retain(ptr %sequence) cold noinline\n"
            "  %owned.idx = call ptr @mule.fast.retain(ptr %index) cold noinline\n"
            f"{item_setup}"
            f"  %result = call ptr {slow}(ptr %owned.seq, ptr %owned.idx, ptr {item_arg}) cold noinline\n"
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
            ""
            if move
            else "  %owned = call ptr @mule.fast.retain(ptr %sequence) cold noinline\n"
        )
        slow_arg = "%sequence" if move else "%owned"
        self.auxiliary.append(
            _helper_comment(f"iteration protocol dispatch; move: {move}")
            + "\n"
            + f"define internal ptr {symbol}(ptr %sequence) {{\n"
            "entry:\n"
            f"  %fast = call ptr {fast_fn}(ptr %sequence)\n"
            "  %ok = icmp ne ptr %fast, null\n"
            f"  br i1 %ok, label %hit, label %slow, {vops.HOT_BRANCH}\n"
            "hit:\n"
            "  ret ptr %fast\n"
            "slow:\n"
            f"{slow_setup}"
            f"  %result = call ptr {slow}(ptr {slow_arg}) cold noinline\n"
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
            _helper_comment("iterator next protocol dispatch")
            + "\n"
            + f"define internal i32 {symbol}(ptr %iterator, ptr %out) {{\n"
            "entry:\n"
            "  %status = call i32 @mule_fast_next_item(ptr %iterator, ptr %out)\n"
            "  %handled = icmp sge i32 %status, 0\n"
            f"  br i1 %handled, label %fast, label %slow, {vops.HOT_BRANCH}\n"
            "fast:\n"
            "  ret i32 %status\n"
            "slow:\n"
            "  %owned = call ptr @mule.fast.retain(ptr %iterator) cold noinline\n"
            f"  %item = call ptr {slow}(ptr %owned) cold noinline\n"
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
            _helper_comment(f"iterator next with unpacking; arity: {arity}")
            + "\n"
            + f"define internal i32 {symbol}(ptr %iterator, ptr %unpack.out, ptr %out) {{\n"
            "entry:\n"
            f"  %status = call i32 @mule_fast_next_unpack(ptr %iterator, i64 {arity}, ptr %unpack.out, ptr %out)\n"
            "  %handled = icmp sge i32 %status, 0\n"
            f"  br i1 %handled, label %fast, label %slow, {vops.HOT_BRANCH}\n"
            "fast:\n"
            "  ret i32 %status\n"
            "slow:\n"
            "  %owned = call ptr @mule.fast.retain(ptr %iterator) cold noinline\n"
            f"  %item = call ptr {slow}(ptr %owned) cold noinline\n"
            "  store ptr %item, ptr %out\n"
            "  %failed = icmp eq ptr %item, null\n"
            "  %result = select i1 %failed, i32 -1, i32 1\n"
            "  ret i32 %result\n"
            "}"
        )
        return symbol

    def cell_ref_helper(self, *, optional: bool = False) -> str:
        key = "$cell.ref.optional" if optional else "$cell.ref"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = "@mule.cell.ref.optional" if optional else "@mule.cell.ref"
        self.protocol_helpers[key] = symbol
        bound_cache = self.attribute_cache()
        value_cache = self.attribute_cache()
        bound_name = self.text("bound")
        value_name = self.text("value")
        unbound = self.protocol_helper("cell_unbound", 1)
        optional_lit = "@mule.true" if optional else "@mule.false"
        self.auxiliary.append(
            f"define internal ptr {symbol}(ptr %cell) {{\n"
            "entry:\n"
            f"  %bound = call ptr @mule.attr.get(ptr %cell, ptr {bound_name}, ptr {bound_cache})\n"
            "  %bound.null = icmp eq ptr %bound, null\n"
            f"  br i1 %bound.null, label %fail, label %bound.ok, {vops.COLD_BRANCH}\n"
            "bound.ok:\n"
            "  %truth = call i32 @mule.fast.truth(ptr %bound)\n"
            "  call void @mule.fast.release(ptr %bound)\n"
            "  %truth.err = icmp slt i32 %truth, 0\n"
            f"  br i1 %truth.err, label %fail, label %truth.ok, {vops.COLD_BRANCH}\n"
            "truth.ok:\n"
            "  %present = icmp ne i32 %truth, 0\n"
            f"  br i1 %present, label %cell.bound, label %cell.unbound, {vops.HOT_BRANCH}\n"
            "cell.bound:\n"
            f"  %val = call ptr @mule.attr.get(ptr %cell, ptr {value_name}, ptr {value_cache})\n"
            "  ret ptr %val\n"
            "cell.unbound:\n"
            f"  %absent = call ptr {unbound}(ptr {optional_lit}) cold noinline\n"
            "  ret ptr %absent\n"
            "fail:\n"
            "  ret ptr null\n"
            "}"
        )
        return symbol

    def cell_set_helper(self) -> str:
        key = "$cell.set"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = "@mule.cell.set"
        self.protocol_helpers[key] = symbol
        value_cache = self.attribute_cache()
        bound_cache = self.attribute_cache()
        value_name = self.text("value")
        bound_name = self.text("bound")
        self.auxiliary.append(
            f"define internal i32 {symbol}(ptr %cell, ptr %value) {{\n"
            "entry:\n"
            f"  %st1 = call i32 @mule.attr.set(ptr %cell, ptr {value_name}, ptr %value, ptr {value_cache})\n"
            "  %err1 = icmp slt i32 %st1, 0\n"
            f"  br i1 %err1, label %fail, label %next, {vops.COLD_BRANCH}\n"
            "next:\n"
            f"  %st2 = call i32 @mule.attr.set(ptr %cell, ptr {bound_name}, ptr @mule.true, ptr {bound_cache})\n"
            "  ret i32 %st2\n"
            "fail:\n"
            "  ret i32 -1\n"
            "}"
        )
        return symbol

    def cell_clear_helper(self) -> str:
        key = "$cell.clear"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = "@mule.cell.clear"
        self.protocol_helpers[key] = symbol
        value_cache = self.attribute_cache()
        bound_cache = self.attribute_cache()
        value_name = self.text("value")
        bound_name = self.text("bound")
        self.auxiliary.append(
            f"define internal i32 {symbol}(ptr %cell) {{\n"
            "entry:\n"
            f"  %st1 = call i32 @mule.attr.set(ptr %cell, ptr {value_name}, ptr @mule.none, ptr {value_cache})\n"
            "  %err1 = icmp slt i32 %st1, 0\n"
            f"  br i1 %err1, label %fail, label %next, {vops.COLD_BRANCH}\n"
            "next:\n"
            f"  %st2 = call i32 @mule.attr.set(ptr %cell, ptr {bound_name}, ptr @mule.false, ptr {bound_cache})\n"
            "  ret i32 %st2\n"
            "fail:\n"
            "  ret i32 -1\n"
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
                    f"  %elem.{idx} = call ptr @mule.fast.tuple.slot(ptr %tuple, i64 {idx})",
                    f"  %slot.{idx} = getelementptr ptr, ptr %out, i64 {idx}",
                    f"  store ptr %elem.{idx}, ptr %slot.{idx}",
                ]
            )
        copies_block = "\n".join(copies)
        self.auxiliary.append(
            _helper_comment(f"unpack into result slots; arity: {arity}")
            + "\n"
            + f"define internal ptr {symbol}(ptr %value, ptr %count, ptr %out) {{\n"
            "entry:\n"
            "  %already = icmp eq ptr %value, null\n"
            "  br i1 %already, label %fast.done, label %check\n"
            "check:\n"
            f"  %matched = call i32 @mule_fast_unpack(ptr %value, i64 {arity}, ptr %out)\n"
            "  %valid = icmp ne i32 %matched, 0\n"
            f"  br i1 %valid, label %fast.done, label %slow, {vops.HOT_BRANCH}\n"
            "fast.done:\n"
            "  call void @mule.fast.release(ptr %count)\n"
            "  ret ptr @mule.none\n"
            "slow:\n"
            f"  %tuple = call ptr {slow}(ptr %value, ptr %count) cold noinline\n"
            "  %bad = icmp eq ptr %tuple, null\n"
            f"  br i1 %bad, label %failed, label %copy, {vops.COLD_BRANCH}\n"
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
            _helper_comment(f"unpack protocol dispatch; arity: {arity}")
            + "\n"
            + f"define internal ptr {symbol}(ptr %value, ptr %count) {{\n"
            "entry:\n"
            f"  %valid = call i1 @mule.fast.tuple.shape(ptr %value, i64 {arity})\n"
            "  br i1 %valid, label %tuple, label %list.check\n"
            "tuple:\n"
            "  call void @mule.fast.release(ptr %count)\n"
            "  ret ptr %value\n"
            "list.check:\n"
            f"  %matched.list = call i32 @mule_list_shape_matches(ptr %value, i64 {arity})\n"
            "  %valid.list = icmp ne i32 %matched.list, 0\n"
            f"  br i1 %valid.list, label %tuple, label %iterable, {vops.HOT_BRANCH}\n"
            "iterable:\n"
            f"  %result = call ptr {slow}(ptr %value, ptr %count) cold noinline\n"
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
        if not self.needs_frame_depth_check(function):
            self.auxiliary.append(
                _helper_comment(f"compiled call; target: {function.name}")
                + "\n"
                + f"define internal ptr {symbol}({params}) {{\n"
                "entry:\n"
                f"  %result = call ptr {body}({params})\n"
                "  ret ptr %result\n"
                "}"
            )
            return symbol
        releases = "\n".join(
            f"  call void @mule.fast.release(ptr %arg.{i}) cold noinline"
            for i in range(arity)
        )
        self.auxiliary.append(
            _helper_comment(f"compiled call with frame check; target: {function.name}")
            + "\n"
            + f"define internal ptr {symbol}({params}) {{\n"
            "entry:\n"
            "  %status = call i32 @mule_enter_frame()\n"
            "  %bad = icmp slt i32 %status, 0\n"
            f"  br i1 %bad, label %failed, label %entered, {vops.COLD_BRANCH}\n"
            "failed:\n" + (releases + "\n" if releases else "") + "  ret ptr null\n"
            "entered:\n"
            f"  %result = call ptr {body}({params})\n"
            "  call void @mule_leave_frame()\n"
            "  ret ptr %result\n"
            "}"
        )
        return symbol

    def positional_call_helper(self, arity: int, *, borrowed: bool = False) -> str:
        key = f"$call.borrowed.{arity}" if borrowed else f"$call.{arity}"
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        symbol = f"@mule.call.borrowed.{arity}" if borrowed else f"@mule.call.{arity}"
        self.protocol_helpers[key] = symbol
        params = ", ".join(["ptr %callee", *(f"ptr %arg.{i}" for i in range(arity))])
        lines = [
            _helper_comment(f"positional call; arity: {arity}; borrowed: {borrowed}"),
            f"define internal ptr {symbol}({params}) {{",
            "entry:",
        ]
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
        target = "@mule_call" if borrowed else "@mule_call_owned"
        lines.append(
            f"  %result = call ptr {target}(ptr %callee, i64 {arity}, ptr {buf})"
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
        lines = [
            _helper_comment(f"resolved method call; arity: {arity}"),
            f"define internal ptr {symbol}({params}) {{",
            "entry:",
        ]
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
        lines = [
            _helper_comment(
                f"keyword call; arguments: {keywords}; resolved: {resolved}"
            ),
            f"define internal ptr {symbol}({', '.join(params)}) {{",
            "entry:",
        ]
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
        supplied: tuple[ValueType, ...] = (),
        *,
        result_contract=None,
    ) -> str:
        supply_types = (
            supplied if len(supplied) == arity + 1 else (UNKNOWN,) * (arity + 1)
        )
        key = (
            "$method.direct",
            functions,
            keywords,
            arity,
            supply_types,
            result_contract,
        )
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
                    if not any(idx < -1 for idx in order):
                        steps = entry_steps(
                            self,
                            function,
                            (supply_types[idx + 1] for idx in order),
                        )
                    else:
                        steps = ((None, ()),)
                    candidates.append((function, order, steps))
            chains.append((bound, shape, candidates))
        targets = tuple(
            f"resolve.{shape}" if candidates else "fallback"
            for _bound, shape, candidates in chains
        )
        builder = _HelperBuilder(self)
        builder.lines = [
            _helper_comment(
                "method call dispatch; candidates: "
                + ", ".join(function.name for function in functions)
            ),
            f"define internal ptr {symbol}({', '.join(params)}) {{",
            "entry:",
            "  %fn.globals.slot = alloca ptr",
            "  %bound = icmp ne ptr %receiver, null",
            f"  br i1 %bound, label %{targets[0]}, label %{targets[1]}",
        ]
        need_failed_block = False

        def emit_return(
            res_name: str, context: EntryContext | None, function, tag: str
        ):
            returned = (
                (
                    self.facts[function].return_type
                    if function in self.facts
                    else function.summary_return_type()
                )
                if context is None
                else context.return_type
            )
            if result_contract is not None and not returned.fits(
                result_contract.checked_type
            ):
                checker = self.return_contract_helper(result_contract)
                builder.line(f"  %null.{tag} = icmp eq ptr {res_name}, null")
                builder.line(
                    f"  br i1 %null.{tag}, label %invoke.failed.{tag}, label %invoke.check.{tag}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"invoke.failed.{tag}:")
                builder.line("  ret ptr null")
                builder.lines.append(f"invoke.check.{tag}:")
                checked = builder.operation(f"call i32 {checker}(ptr {res_name})")
                rejected = builder.operation(f"icmp slt i32 {checked}, 0")
                builder.line(
                    f"  br i1 {rejected}, label %invoke.incompatible.{tag}, label %invoke.accepted.{tag}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"invoke.incompatible.{tag}:")
                builder.line(
                    f"  call void @mule.fast.release(ptr {res_name}) cold noinline"
                )
                builder.line("  ret ptr null")
                builder.lines.append(f"invoke.accepted.{tag}:")
            if returned is not UNKNOWN:
                assumed = returned.accept(_TypeAssumption(builder, res_name))
                if assumed is not None:
                    missing = builder.operation(f"icmp eq ptr {res_name}, null")
                    assumed = builder.operation(f"or i1 {missing}, {assumed}")
                    builder.line(f"  call void @llvm.assume(i1 {assumed})")
            builder.line(f"  ret ptr {res_name}")

        def emit_invoke(function, order, steps, variant_index, bound):
            nonlocal need_failed_block
            builder.lines.append(f"direct.{variant_index}:")
            available = []
            for pos, idx in enumerate(order):
                if idx < -1:
                    builder.lines.extend(
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
                    builder.line(f"  {combined} = and i1 {ready}, {present}")
                    ready = combined
                builder.lines.extend(
                    [
                        f"  br i1 {ready}, label %defaults.ready.{variant_index}, label %fallback, {vops.HOT_BRANCH}",
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
                builder.lines.extend(
                    [
                        f"  %append.res.{variant_index} = call ptr @mule_storage_append(ptr %receiver, ptr %arg.0)",
                        "  call void @mule.fast.release(ptr %receiver)",
                        "  call void @mule.fast.release(ptr %arg.0)",
                    ]
                )
                emit_return(
                    f"%append.res.{variant_index}",
                    None,
                    function,
                    f"{variant_index}.append",
                )
                return
            if (
                bound
                and function.name == "mule_sequences_mule_fn_ListStorage.pop"
                and order == (-1, -2)
            ):
                builder.lines.extend(
                    [
                        f"  %pop.res.{variant_index} = call ptr @mule_storage_pop(ptr %receiver)",
                        "  call void @mule.fast.release(ptr %receiver)",
                    ]
                )
                emit_return(
                    f"%pop.res.{variant_index}",
                    None,
                    function,
                    f"{variant_index}.pop",
                )
                return
            check_depth = self.needs_frame_depth_check(function)
            if check_depth:
                need_failed_block = True

            def emit_step_invoke(context: EntryContext | None, step_idx: int) -> None:
                tag = f"{variant_index}.{step_idx}"
                if check_depth:
                    entered_label = f"entered.{tag}"
                    builder.lines.extend(
                        [
                            f"  %frame.{tag} = call i32 @mule_enter_frame()",
                            f"  %rejected.{tag} = icmp slt i32 %frame.{tag}, 0",
                            f"  br i1 %rejected.{tag}, label %failed, label %{entered_label}, {vops.COLD_BRANCH}",
                            f"{entered_label}:",
                        ]
                    )
                builder.line(f"  %globals.{tag} = load ptr, ptr %fn.globals.slot")
                owned = []
                for pos, idx in enumerate(order):
                    if idx == -1:
                        owned.append("%receiver")
                    elif idx >= 0:
                        owned.append(f"%arg.{idx}")
                    else:
                        builder.line(
                            f"  %own.{tag}.{pos} = call ptr @mule.fast.retain(ptr %def.{variant_index}.{pos})"
                        )
                        owned.append(f"%own.{tag}.{pos}")
                closure_ops = []
                if self.closure_of(function):
                    builder.line(
                        f"  %closure.{tag} = call ptr @mule_function_closure_of(ptr %callable)"
                    )
                    closure_ops.append(f"ptr %closure.{tag}")
                body_sym = (
                    self.specialized[function, context.id]
                    if context is not None
                    else self.bodies[function]
                )
                body_args = ", ".join(
                    [
                        f"ptr %globals.{tag}",
                        *closure_ops,
                        *(f"ptr {v}" for v in owned),
                    ]
                )
                builder.line(f"  %direct.res.{tag} = call ptr {body_sym}({body_args})")
                if check_depth:
                    builder.line("  call void @mule_leave_frame()")
                emit_return(f"%direct.res.{tag}", context, function, tag)

            for step_idx, (context, needed) in enumerate(steps):
                guards = [
                    expected.accept(
                        _ArgumentGuard(
                            builder,
                            "%receiver"
                            if order[param_idx] == -1
                            else f"%arg.{order[param_idx]}",
                        )
                    )
                    for param_idx, expected in enumerate(needed)
                    if expected is not None
                ]
                if not guards:
                    emit_step_invoke(context, step_idx)
                    break
                valid = guards[0]
                for guard in guards[1:]:
                    valid = builder.operation(f"and i1 {valid}, {guard}")
                typed = f"typed.{variant_index}.{step_idx}"
                next_label = f"next.{variant_index}.{step_idx}"
                builder.line(f"  br i1 {valid}, label %{typed}, label %{next_label}")
                builder.lines.append(f"{typed}:")
                emit_step_invoke(context, step_idx)
                builder.lines.append(f"{next_label}:")

        for chain_index, (bound, shape, candidates) in enumerate(chains):
            if not candidates:
                continue
            builder.lines.extend(
                [
                    f"resolve.{shape}:",
                    f"  %entry.{shape} = call ptr @mule_method_entry(ptr %callable, ptr %fn.globals.slot)",
                    f"  br label %check.{shape}.0",
                ]
            )
            for candidate_index, (function, order, steps) in enumerate(candidates):
                variant_index = 2 * candidate_index + chain_index
                next_label = (
                    f"check.{shape}.{candidate_index + 1}"
                    if candidate_index + 1 < len(candidates)
                    else "fallback"
                )
                branch_weight = (
                    f", {vops.HOT_BRANCH}" if next_label == "fallback" else ""
                )
                builder.lines.extend(
                    [
                        f"check.{shape}.{candidate_index}:",
                        f"  %match.{shape}.{candidate_index} = icmp eq ptr %entry.{shape}, {self.functions[function]}",
                        f"  br i1 %match.{shape}.{candidate_index}, label %direct.{variant_index}, label %{next_label}{branch_weight}",
                    ]
                )
                emit_invoke(function, order, steps, variant_index, bound)
        if need_failed_block:
            builder.lines.extend(
                [
                    "failed:",
                    "  call void @mule.fast.release(ptr %receiver) cold noinline",
                ]
            )
            for i in range(arity):
                builder.line(
                    f"  call void @mule.fast.release(ptr %arg.{i}) cold noinline"
                )
            builder.line("  ret ptr null")
        builder.lines.extend(
            [
                "fallback:",
                f"  %fallback.res = call ptr {fallback_helper}({fallback_args}) cold noinline",
            ]
        )
        if result_contract is not None:
            checker = self.return_contract_helper(result_contract)
            builder.lines.extend(
                [
                    "  %fallback.null = icmp eq ptr %fallback.res, null",
                    f"  br i1 %fallback.null, label %fallback.err, label %fallback.check, {vops.COLD_BRANCH}",
                    "fallback.err:",
                    "  ret ptr null",
                    "fallback.check:",
                ]
            )
            checked = builder.operation(
                f"call i32 {checker}(ptr %fallback.res) cold noinline"
            )
            rejected = builder.operation(f"icmp slt i32 {checked}, 0")
            builder.line(
                f"  br i1 {rejected}, label %fallback.incompatible, label %fallback.ok, {vops.COLD_BRANCH}"
            )
            builder.lines.extend(
                [
                    "fallback.incompatible:",
                    "  call void @mule.fast.release(ptr %fallback.res) cold noinline",
                    "  ret ptr null",
                    "fallback.ok:",
                ]
            )
        builder.lines.extend(
            [
                "  ret ptr %fallback.res",
                "}",
            ]
        )
        self.auxiliary.append("\n".join(builder.lines))
        return symbol

    def direct_call_helper(
        self,
        function: Function,
        keywords: tuple[str | None, ...],
        arity: int,
        steps: tuple[tuple[EntryContext | None, tuple[ValueType | None, ...]], ...],
        *,
        proven: bool = False,
        same_namespace: bool = False,
        receiver_call: bool = False,
        borrowed_arguments: frozenset[int] = frozenset(),
        local_environment: bool = False,
        result_contract=None,
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
            same_namespace,
            receiver_call,
            borrowed_arguments,
            local_environment,
            result_contract,
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
        body_globals = "%globals" if proven and same_namespace else "%callee.globals"
        if proven and same_namespace:
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
            ]
        elif proven:
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
                    f"  br i1 %matching.bound, label %direct, label %fallback, {vops.HOT_BRANCH}",
                    "direct:",
                ]
            else:
                builder.lines += [
                    f"  br i1 %matching, label %direct, label %fallback, {vops.HOT_BRANCH}",
                    "direct:",
                ]
            builder.line("  %callee.globals = load ptr, ptr %fn.globals.slot")

        check_depth = self.needs_frame_depth_check(function)

        def emit_invoke(context: EntryContext | None, tag: int) -> None:
            if borrowed_arguments:
                borrowed_params = frozenset(
                    pos for pos, idx in enumerate(order) if idx in borrowed_arguments
                )
                context = self.borrowed_context(function, context, borrowed_params)
            if check_depth:
                entered = f"entered.{tag}"
                builder.line("  %frame." + str(tag) + " = call i32 @mule_enter_frame()")
                builder.line(f"  %rejected.{tag} = icmp slt i32 %frame.{tag}, 0")
                builder.line(
                    f"  br i1 %rejected.{tag}, label %failed, label %{entered}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"{entered}:")
            owned = []
            for pos, idx in enumerate(order):
                if idx >= 0:
                    owned.append(f"%arg.{idx}")
                else:
                    builder.line(
                        f"  %own.{tag}.{pos} = call ptr @mule.fast.retain(ptr %def.{pos})"
                    )
                    owned.append(f"%own.{tag}.{pos}")
            closure_ops = []
            if self.closure_of(function):
                if local_environment:
                    closure_ops.append("ptr %callee")
                else:
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
            if check_depth:
                builder.line("  call void @mule_leave_frame()")
            returned = (
                (
                    self.facts[function].return_type
                    if function in self.facts
                    else function.summary_return_type()
                )
                if context is None
                else context.return_type
            )
            if result_contract is not None and not returned.fits(
                result_contract.checked_type
            ):
                checker = self.return_contract_helper(result_contract)
                builder.line(f"  %null.{tag} = icmp eq ptr %res.{tag}, null")
                builder.line(
                    f"  br i1 %null.{tag}, label %invoke.failed.{tag}, label %invoke.check.{tag}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"invoke.failed.{tag}:")
                builder.line("  ret ptr null")
                builder.lines.append(f"invoke.check.{tag}:")
                checked = builder.operation(f"call i32 {checker}(ptr %res.{tag})")
                rejected = builder.operation(f"icmp slt i32 {checked}, 0")
                builder.line(
                    f"  br i1 {rejected}, label %invoke.incompatible.{tag}, label %invoke.accepted.{tag}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"invoke.incompatible.{tag}:")
                builder.line(
                    f"  call void @mule.fast.release(ptr %res.{tag}) cold noinline"
                )
                builder.line("  ret ptr null")
                builder.lines.append(f"invoke.accepted.{tag}:")
            if context is not None:
                assumed = context.return_type.accept(
                    _TypeAssumption(builder, f"%res.{tag}")
                )
                if assumed is not None:
                    missing = builder.operation(f"icmp eq ptr %res.{tag}, null")
                    assumed = builder.operation(f"or i1 {missing}, {assumed}")
                    builder.line(f"  call void @llvm.assume(i1 {assumed})")
            builder.line(f"  ret ptr %res.{tag}")

        for pos, idx in enumerate(order):
            if idx < 0:
                builder.line(
                    f"  %def.{pos} = call ptr @mule_function_default(ptr %callee, i32 {-2 - idx})"
                )
        for step_idx, (context, needed) in enumerate(steps):
            guards = [
                expected.accept(
                    _ArgumentGuard(
                        builder,
                        f"%arg.{order[param_idx]}"
                        if order[param_idx] >= 0
                        else f"%def.{param_idx}",
                    )
                )
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

        if check_depth:
            builder.lines.append("failed:")
            for i in range(arity):
                if i not in borrowed_arguments:
                    builder.line(
                        f"  call void @mule.fast.release(ptr %arg.{i}) cold noinline"
                    )
            builder.line("  ret ptr null")
        if not proven:
            builder.lines.append("fallback:")
            for index in borrowed_arguments:
                builder.line(f"  call ptr @mule.fast.retain(ptr %arg.{index})")
            builder.line(
                f"  %fallback.res = call ptr {fallback_helper}({fallback_args})"
            )
            if result_contract is not None:
                checker = self.return_contract_helper(result_contract)
                builder.lines.extend(
                    [
                        "  %fallback.null = icmp eq ptr %fallback.res, null",
                        f"  br i1 %fallback.null, label %fallback.err, label %fallback.check, {vops.COLD_BRANCH}",
                        "fallback.err:",
                        "  ret ptr null",
                        "fallback.check:",
                    ]
                )
                checked = builder.operation(f"call i32 {checker}(ptr %fallback.res)")
                rejected = builder.operation(f"icmp slt i32 {checked}, 0")
                builder.line(
                    f"  br i1 {rejected}, label %fallback.incompatible, label %fallback.ok, {vops.COLD_BRANCH}"
                )
                builder.lines.extend(
                    [
                        "fallback.incompatible:",
                        "  call void @mule.fast.release(ptr %fallback.res) cold noinline",
                        "  ret ptr null",
                        "fallback.ok:",
                    ]
                )
            builder.line("  ret ptr %fallback.res")
        builder.lines.append("}")
        self.auxiliary.append(
            _helper_comment(f"direct call; target: {function.name}")
            + "\n"
            + "\n".join(builder.lines)
        )
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
        builder.lines = [
            _helper_comment("return contract check"),
            f"define internal i32 {symbol}(ptr %value) {{",
            "entry:",
        ]
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
        local_environment: bool = False,
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
            local_environment,
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
        body_globals = "%globals" if local_environment else "%callee.globals"
        if local_environment:
            builder.lines = [
                f"define internal ptr {symbol}({', '.join(params)}) {{",
                "entry:",
            ]
        elif proven:
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
                f"  br i1 %matching, label %direct, label %fallback, {vops.HOT_BRANCH}",
                "direct:",
                "  %callee.globals = load ptr, ptr %fn.globals.slot",
            ]

        check_depth = self.needs_frame_depth_check(function)

        def emit_invoke(context: EntryContext | None, tag: int) -> None:
            if check_depth:
                entered = f"entered.{tag}"
                builder.line("  %frame." + str(tag) + " = call i32 @mule_enter_frame()")
                builder.line(f"  %rejected.{tag} = icmp slt i32 %frame.{tag}, 0")
                builder.line(
                    f"  br i1 %rejected.{tag}, label %failed, label %{entered}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"{entered}:")
            owned = []
            for pos, idx in enumerate(order):
                if idx >= 0:
                    owned.append(f"%arg.{idx}")
                else:
                    builder.line(
                        f"  %own.{tag}.{pos} = call ptr @mule.fast.retain(ptr %def.{pos})"
                    )
                    owned.append(f"%own.{tag}.{pos}")
            closure_ops = []
            if self.closure_of(function):
                if local_environment:
                    closure_ops.append("ptr %callee")
                else:
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
            if check_depth:
                builder.line("  call void @mule_leave_frame()")
            returned = (
                (
                    self.facts[function].return_type
                    if function in self.facts
                    else function.summary_return_type()
                )
                if context is None
                else context.return_type
            )
            if result_contract is not None and not returned.fits(result_contract.fact):
                checker = self.return_contract_helper(
                    result_contract, unpacked_arity=unpack_arity
                )
                builder.line(f"  %null.{tag} = icmp eq ptr %res.{tag}, null")
                builder.line(
                    f"  br i1 %null.{tag}, label %invoke.failed.{tag}, label %invoke.check.{tag}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"invoke.failed.{tag}:")
                builder.line("ret ptr null")
                builder.lines.append(f"invoke.check.{tag}:")
                checked = builder.operation(f"call i32 {checker}(ptr %tuple.out)")
                rejected = builder.operation(f"icmp slt i32 {checked}, 0")
                builder.line(
                    f"br i1 {rejected}, label %invoke.incompatible.{tag}, label %invoke.accepted.{tag}, {vops.COLD_BRANCH}"
                )
                builder.lines.append(f"invoke.incompatible.{tag}:")
                for slot_index in range(unpack_arity):
                    slot = builder.operation(
                        f"getelementptr ptr, ptr %tuple.out, i64 {slot_index}"
                    )
                    value = builder.operation(f"load ptr, ptr {slot}")
                    builder.line(f"store ptr null, ptr {slot}")
                    builder.line(
                        f"call void @mule.fast.release(ptr {value}) cold noinline"
                    )
                builder.line("ret ptr null")
                builder.lines.append(f"invoke.accepted.{tag}:")
            builder.line(f"  ret ptr %res.{tag}")

        for pos, idx in enumerate(order):
            if idx < 0:
                builder.line(
                    f"  %def.{pos} = call ptr @mule_function_default(ptr %callee, i32 {-2 - idx})"
                )
        for step_idx, (context, needed) in enumerate(steps):
            guards = [
                expected.accept(
                    _ArgumentGuard(
                        builder,
                        f"%arg.{order[param_idx]}"
                        if order[param_idx] >= 0
                        else f"%def.{param_idx}",
                    )
                )
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

        if check_depth:
            builder.lines.append("failed:")
            for i in range(arity):
                builder.line(
                    f"  call void @mule.fast.release(ptr %arg.{i}) cold noinline"
                )
            builder.line("  ret ptr null")
        if not proven:
            builder.lines.extend(
                [
                    "fallback:",
                    f"  %fallback.res = call ptr {fallback_helper}({fallback_args})",
                    "  %fallback.null = icmp eq ptr %fallback.res, null",
                    f"  br i1 %fallback.null, label %fallback.err, label %fallback.check, {vops.COLD_BRANCH}",
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
                    f"br i1 {rejected}, label %fallback.incompatible, label %fallback.unpack, {vops.COLD_BRANCH}"
                )
                builder.lines.append("fallback.incompatible:")
                builder.line(
                    "call void @mule.fast.release(ptr %fallback.res) cold noinline"
                )
                builder.line("ret ptr null")
            else:
                builder.line("br label %fallback.unpack")
            builder.lines.extend(
                [
                    "fallback.unpack:",
                    f"  %unpacked = call ptr {unpack_helper}(ptr %fallback.res, ptr inttoptr (i64 {encoded_arity} to ptr))",
                    "  %unpacked.null = icmp eq ptr %unpacked, null",
                    f"  br i1 %unpacked.null, label %fallback.err, label %fallback.slots, {vops.COLD_BRANCH}",
                    "fallback.slots:",
                ]
            )
            for slot_idx in range(unpack_arity):
                builder.lines.extend(
                    [
                        f"  %slot.val.{slot_idx} = call ptr @mule.fast.tuple.slot(ptr %unpacked, i64 {slot_idx})",
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
        self.auxiliary.append(
            _helper_comment(f"unpacked direct call; target: {function.name}")
            + "\n"
            + "\n".join(builder.lines)
        )
        return symbol

    def contexts(self, lambda_: Lambda):
        """The general entry context and the guarded variants."""
        facts = self.facts[lambda_]
        borrowed_params = (
            frozenset(range(len(lambda_.vars)))
            if any(
                self.sequence_functions.get(name) is lambda_
                for name in _BORROWED_SEQUENCE_PROTOCOLS
            )
            else frozenset()
        )
        general = EntryContext(
            None,
            facts.types,
            facts.targets,
            facts.parameters or (UNKNOWN,) * len(lambda_.vars),
            facts.return_type,
            borrowed_parameters=borrowed_params,
            borrowed_callees=self.borrowed_callees,
            element_contracts=facts.element_contracts,
            in_bounds_nodes=facts.in_bounds_nodes,
            escape_facts=self.escape_facts,
            local_environments=self.local_environments,
        )
        variants = [
            EntryContext(
                context.id,
                context.types,
                context.targets,
                context.parameters,
                context.return_type,
                borrowed_parameters=borrowed_params,
                borrowed_callees=self.borrowed_callees,
                element_contracts=context.element_contracts,
                in_bounds_nodes=context.in_bounds_nodes,
                escape_facts=self.escape_facts,
                local_environments=self.local_environments,
            )
            for context in facts.variants
        ]
        if facts.final_bindings and self.native_plans.get(lambda_) is None:
            general = self.final_ready_context(lambda_, general, facts)
            variants = [
                self.final_ready_context(lambda_, context, facts)
                for context in variants
            ]
        return general, variants

    def final_ready_context(self, function, context, facts):
        fallback_id = "fallback.final." + str(context.id or "general")
        fallback = replace(context, id=fallback_id)
        self.backend_contexts[function, fallback_id] = fallback
        types, targets = facts.ready_final_facts(context.types, context.targets)
        return replace(
            context,
            types=types,
            targets=targets,
            final_bindings=facts.final_bindings,
            final_fallback=fallback_id,
        )

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

    def numeric_entry(
        self,
        function: Lambda,
        context: EntryContext,
        borrowed_parameters: frozenset[int] = frozenset(),
    ) -> NumericEntry:
        """Select an internal ABI without changing the entry's semantic facts."""
        return numeric_entry(
            self,
            function,
            context,
            borrowed_parameters,
            probe_component=lambda fn, ctx: compile_component(
                fn, ctx, emit_coercions=False
            ),
        )

    def guarded_numeric_call_helper(self, entry: NumericEntry) -> str:
        convention = linked(entry.context.convention, entry, "convention")
        key = (
            "$call.numeric",
            entry.function,
            entry.context.id,
            entry.context.borrowed_parameters,
            convention.parameters,
            convention.returned,
            entry.result_contract,
        )
        existing = self.protocol_helpers.get(key)
        if existing is not None:
            return existing
        index = len(self.protocol_helpers)
        arity = len(convention.parameters)
        symbol = _symbol(f"mule.numeric.{entry.function.name}.{arity}.{index}")
        self.protocol_helpers[key] = symbol
        needs_error_flag = convention.returned is not DESCRIPTOR
        params = [
            "ptr %callee",
            *(["ptr %numeric.error"] if needs_error_flag else []),
            *(f"{sc.llvm_type} %arg.{i}" for i, sc in enumerate(convention.parameters)),
        ]
        ret_type = convention.returned.llvm_type
        zero = (
            "0.0"
            if convention.returned is DOUBLE
            else "0"
            if convention.returned is SIGNED
            else "null"
        )
        fallback_helper = self.positional_call_helper(arity)
        builder = _HelperBuilder(self)
        check_depth = self.needs_frame_depth_check(entry.function)
        owned_descriptors = [
            i
            for i, sc in enumerate(convention.parameters)
            if sc is DESCRIPTOR and i not in entry.context.borrowed_parameters
        ]
        frame_fail_label = "frame.failed" if owned_descriptors else "failed"
        builder.lines = [
            _helper_comment(
                f"guarded numeric call; target: {entry.function.name}; context: {entry.context.id}"
            ),
            f"define internal {ret_type} {symbol}({', '.join(params)}) {{",
            "entry:",
            "  %fn.globals.slot = alloca ptr",
            "  %callee.entry = call ptr @mule_method_entry(ptr %callee, ptr %fn.globals.slot)",
            f"  %matching = icmp eq ptr %callee.entry, {self.functions[entry.function]}",
            f"  br i1 %matching, label %direct, label %fallback, {vops.HOT_BRANCH}",
            "direct:",
        ]
        if check_depth:
            builder.lines.extend(
                [
                    "  %frame = call i32 @mule_enter_frame()",
                    "  %frame.bad = icmp slt i32 %frame, 0",
                    f"  br i1 %frame.bad, label %{frame_fail_label}, label %entered, {vops.COLD_BRANCH}",
                ]
            )
            if owned_descriptors:
                builder.lines.append("frame.failed:")
                for i in owned_descriptors:
                    builder.lines.append(
                        f"  call void @mule.fast.release(ptr %arg.{i})"
                    )
                builder.lines.append("  br label %failed")
            builder.lines.append("entered:")
        builder.lines.append("  %callee.globals = load ptr, ptr %fn.globals.slot")
        closure_args = []
        if self.closure_of(entry.function):
            builder.lines.append(
                "  %closure = call ptr @mule_function_closure_of(ptr %callee)"
            )
            closure_args.append("ptr %closure")
        direct_args = [
            "ptr %callee.globals",
            *(["ptr %numeric.error"] if needs_error_flag else []),
            *closure_args,
            *(f"{sc.llvm_type} %arg.{i}" for i, sc in enumerate(convention.parameters)),
        ]
        builder.lines.append(
            f"  %direct.res = call {ret_type} {entry.symbol}({', '.join(direct_args)})"
        )
        if check_depth:
            builder.lines.append("  call void @mule_leave_frame()")
        builder.lines.extend(
            [
                f"  ret {ret_type} %direct.res",
                "fallback:",
            ]
        )
        for index in entry.context.borrowed_parameters:
            builder.line(
                f"  call ptr @mule.fast.retain(ptr %arg.{index}) cold noinline"
            )
        boxed_args = []
        for i, sc in enumerate(convention.parameters):
            if sc is DOUBLE:
                builder.line(
                    f"  %box.{i} = call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double %arg.{i}) cold noinline"
                )
                boxed_args.append(f"ptr %box.{i}")
            elif sc is SIGNED:
                builder.line(f"  %shl.{i} = shl i64 %arg.{i}, 1")
                builder.line(f"  %tag.{i} = or disjoint i64 %shl.{i}, 1")
                builder.line(f"  %box.{i} = inttoptr i64 %tag.{i} to ptr")
                boxed_args.append(f"ptr %box.{i}")
            else:
                boxed_args.append(f"ptr %arg.{i}")
        fallback_call_args = ", ".join(["ptr %callee", *boxed_args])
        builder.lines.extend(
            [
                f"  %fallback.res = call ptr {fallback_helper}({fallback_call_args}) cold noinline",
                "  %fallback.null = icmp eq ptr %fallback.res, null",
                f"  br i1 %fallback.null, label %failed, label %fallback.check, {vops.COLD_BRANCH}",
                "fallback.check:",
            ]
        )
        if entry.result_contract is not None:
            checker = self.return_contract_helper(entry.result_contract)
            checked = builder.operation(
                f"call i32 {checker}(ptr %fallback.res) cold noinline"
            )
            rejected = builder.operation(f"icmp slt i32 {checked}, 0")
            builder.lines.extend(
                [
                    f"  br i1 {rejected}, label %fallback.incompatible, label %fallback.ok, {vops.COLD_BRANCH}",
                    "fallback.incompatible:",
                    "  call void @mule.fast.release(ptr %fallback.res) cold noinline",
                    "  br label %failed",
                    "fallback.ok:",
                ]
            )
        if convention.returned is DOUBLE:
            builder.lines.extend(
                [
                    f"  %unboxed = call double @{FLOAT_BOX_LAYOUT.symbol('load')}(ptr %fallback.res)",
                    "  call void @mule.fast.release(ptr %fallback.res)",
                    "  ret double %unboxed",
                ]
            )
        elif convention.returned is SIGNED:
            builder.lines.extend(
                [
                    "  %bits = ptrtoint ptr %fallback.res to i64",
                    "  %unboxed = ashr i64 %bits, 1",
                    "  ret i64 %unboxed",
                ]
            )
        else:
            builder.lines.append("  ret ptr %fallback.res")
        builder.lines.append("failed:")
        if needs_error_flag:
            builder.lines.append("  store i1 true, ptr %numeric.error")
        builder.lines.extend(
            [
                f"  ret {ret_type} {zero}",
                "}",
            ]
        )
        self.auxiliary.append("\n".join(builder.lines))
        return symbol

    def select_numeric_calls(self, component) -> bool:
        """Use scalar locations for proven or guarded calls and compatible consumers."""
        return select_numeric_calls(self, component)

    def fresh_box_result(self, component, vop):
        """Transfer fresh object ownership only across proven factory calls."""
        if vop.info not in (vops.FULL_CALL, vops.LOCAL_EXTENT_CALL):
            return None
        site = vop.codegen_info
        target = site.target
        call_args = vop.args[1:] if site.callee_required else vop.args
        if (
            not site.proven
            or site.keywords
            or target not in self.owned
            or target is component.lambda_
            or target.call_order((), len(call_args)) != tuple(range(len(target.vars)))
        ):
            return None
        layout = vop.results[0].tn.primitive_type.boxed_layout
        if layout is None:
            return None
        sources = tuple(ref.tn.type or UNKNOWN for ref in call_args)
        general, variants = self.contexts(target)
        contexts = {None: general, **{context.id: context for context in variants}}
        for selected, _guards in entry_steps(self, target, sources):
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
            declared = _declared_protocol_context(lambda_, self.facts[lambda_])
            if declared is not None:
                # Imported callers need this entry even without a local call.
                self.specialized[lambda_, declared]
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
                "native",
                facts.types,
                facts.targets,
                (),
                facts.return_type,
                plan,
                escape_facts=self.escape_facts,
                local_environments=self.local_environments,
            )
            functions.append(self.emit_body(lambda_, context))
        functions.extend(
            native_callbacks(self, layout) for layout in self.layouts.values()
        )
        owned_imports = [
            info for info in self.imports.values() if self.owns_import(info)
        ]
        functions.extend(self.import_initializer(info) for info in owned_imports)
        functions.extend(
            self.missing_import_initializer(info)
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
        if program.interface_bytes is not None:
            iface_sym = _symbol(f"__mule_interface:{program.unit}")
            iface_hex = "".join(map(_HEX_BYTE.__getitem__, program.interface_bytes))
            self.auxiliary.append(
                f"{iface_sym} = constant [{len(program.interface_bytes) + 1} x i8] "
                f'c"{iface_hex}\\00", section ".mule.interface", align 1'
            )
            self.auxiliary.append(
                f"@llvm.used = appending global [1 x ptr] [ptr {iface_sym}], "
                'section "llvm.metadata"'
            )
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
        body_code = _without_llvm_comments(body_text)
        referenced = set(_MULE_SYMBOL_RE.findall(body_code))
        for llvm_sym in (
            "@llvm.assume",
            "@llvm.sadd.with.overflow.i64",
            "@llvm.ssub.with.overflow.i64",
            "@llvm.smul.with.overflow.i64",
        ):
            if llvm_sym in body_code:
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
                    search_text += "\n" + _without_llvm_comments(chunk)
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
            _helper_comment(f"compiled import registration; unit: {self.program.unit}")
            + "\n"
            + f"define internal void {entry}() {{\nentry:\n"
            + "\n".join(calls)
            + "\n  ret void\n}",
        ]

    def attribute_lookup_adapter(self) -> str | None:
        """Borrowed native arguments enter the owned compiled lookup protocol."""
        target = self.sequence_functions.get("get_attribute")
        if target not in self.owned:
            return None
        return (
            _helper_comment(f"attribute lookup adapter; target: {target.name}")
            + "\n"
            + "define ptr @mule.attribute.lookup(ptr %receiver, ptr %name) {\n"
            "entry:\n"
            "  %frame = call i32 @mule_enter_frame()\n"
            "  %bad = icmp slt i32 %frame, 0\n"
            f"  br i1 %bad, label %failed, label %entered, {vops.COLD_BRANCH}\n"
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
            _helper_comment(f"unit initialization; unit: {self.program.unit}")
            + "\n"
            + f"define {linkage}ptr {unit_initializer(self.program.unit)}() {{\n"
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
            _helper_comment(f"unit finalization; unit: {self.program.unit}")
            + "\n"
            + f"define {linkage}void {unit_finalizer(self.program.unit)}() {{\n"
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
                f"  br i1 %unit.bad.{index}, label %failure, label %unit.ready.{index}, {vops.COLD_BRANCH}",
                f"unit.ready.{index}:",
            ]
        definitions.append(
            _helper_comment(f"protocol startup; unit: {program.unit}")
            + "\n"
            + "define internal ptr @mule.initialize.protocols(ptr %globals, ptr %args) {\n"
            "entry:\n"
            + "\n".join(startup)
            + "\n"
            + f"  %sequence = call ptr {self.functions[linked(program.protocol_entry, program, 'protocol entry')]}(ptr %globals, ptr %args)\n"
            "  %failed = icmp eq ptr %sequence, null\n"
            "  call void @mule_release(ptr %sequence)\n"
            f"  br i1 %failed, label %failure, label %print, {vops.COLD_BRANCH}\n"
            "print:\n"
            + (
                f"  %format = call ptr {self.functions[linked(program.format_entry, program, 'format entry')]}(ptr %globals, ptr %args)\n"
                "  %format.failed = icmp eq ptr %format, null\n"
                "  call void @mule_release(ptr %format)\n"
                f"  br i1 %format.failed, label %failure, label %format.ready, {vops.COLD_BRANCH}\n"
                "format.ready:\n"
                if program.format_entry is not None
                else ""
            )
            + f"  %result = call ptr {self.functions[linked(program.print_entry, program, 'print entry')]}(ptr %globals, ptr %args)\n"
            "  %print.failed = icmp eq ptr %result, null\n"
            f"  br i1 %print.failed, label %failure, label %unit, {vops.COLD_BRANCH}\n"
            "unit:\n"
            "  call void @mule_set_attribute_lookup(ptr @mule.attribute.lookup)\n"
            f"  %own = call ptr {unit_initializer(program.unit)}()\n"
            "  %own.failed = icmp eq ptr %own, null\n"
            f"  br i1 %own.failed, label %own.failure, label %ready, {vops.COLD_BRANCH}\n"
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
                _helper_comment(
                    f"argument constructor adapter; target: {program.argument_constructor.name}"
                )
                + "\n"
                + "define internal ptr @mule.arguments.construct(ptr %class, ptr %view) {\n"
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
        toolchain_call = ""
        if program.toolchain_info is not None:
            py_host, clang_cmd, rustc_cmd, opt_lvl = program.toolchain_info
            toolchain_call = (
                f"  call void @mule_set_image_toolchain(ptr {self.text(py_host)}, "
                f"ptr {self.text(clang_cmd)}, ptr {self.text(rustc_cmd)}, i64 {opt_lvl})\n"
            )
        definitions.append(
            _helper_comment(f"executable entry; unit: {program.unit}")
            + "\n"
            + "define i32 @main(i32 %argc, ptr %argv) {\n"
            "entry:\n"
            "  call void @mule_set_process_arguments(i32 %argc, ptr %argv)\n"
            + f"  call void @mule_set_image_search_path(ptr {paths}, i64 {len(program.search_paths)})\n"
            + toolchain_call
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
            select_tuple_delivery(
                component, self.return_tuple_arities, self.method_targets
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
            f"  br i1 %unit.bad, label %unit.failed, label %unit.ready, {vops.COLD_BRANCH}",
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
                _helper_comment(f"missing import lookup; module: {info.module_name}"),
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
        linkage = "weak_odr " if info.native else ""
        cache = self.import_cache(info)
        self.auxiliary.append(f"{cache} = {linkage or 'internal '}global ptr null")
        legacy = [
            _helper_comment(f"import initialization; module: {info.module_name}"),
            f"define {linkage}ptr {symbol}() {{",
            "entry:",
            f"  %cached = load ptr, ptr {cache}",
            "  %ready = icmp ne ptr %cached, null",
            f"  br i1 %ready, label %existing, label %initialize, {vops.HOT_BRANCH}",
            "existing:",
            f"  %owned = call ptr @mule_import_loaded(ptr {self.text(info.module_name)})",
            "  ret ptr %owned",
            "initialize:",
            f"  %globals = call ptr {self.import_create_symbol(info)}()",
            "  %bad.globals = icmp eq ptr %globals, null",
            f"  br i1 %bad.globals, label %failed, label %created, {vops.COLD_BRANCH}",
            "created:",
            f"  call void @mule_module_loaded(ptr {self.text(info.module_name)}, ptr %globals)",
            f"  %result = call ptr {self.import_execute_symbol(info)}(ptr %globals)",
            "  call void @mule_release(ptr %globals)",
            "  %bad.result = icmp eq ptr %result, null",
            "  call void @mule_release(ptr %result)",
            f"  br i1 %bad.result, label %failed, label %finished, {vops.COLD_BRANCH}",
            "finished:",
            f"  %module = call ptr @mule_import_loaded(ptr {self.text(info.module_name)})",
            "  ret ptr %module",
            "failed:",
            f"  call void @mule_module_failed(ptr {self.text(info.module_name)})",
            "  ret ptr null",
            "}",
        ]
        lines = [
            _helper_comment(f"import namespace creation; module: {info.module_name}"),
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
                    f"  br i1 %bad.{label}, label %failed, label %after.{label}, {vops.COLD_BRANCH}",
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
                    f"  br i1 %bad.{label}, label %failed, label %after.{label}, {vops.COLD_BRANCH}",
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
                    f"  br i1 %bad.path, label %failed, label %after.path, {vops.COLD_BRANCH}",
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
            _helper_comment(
                f"import execution; module: {info.module_name}; source: {info.filename}"
            ),
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
                    f"  br i1 %bad.frame, label %failed, label %execute, {vops.COLD_BRANCH}",
                    "execute:",
                    f"  %result = call ptr {self.bodies_by_name(info.initializer)}(ptr %globals)",
                    "  call void @mule_leave_frame()",
                ]
            )
        lines.extend(
            [
                "  %bad.result = icmp eq ptr %result, null",
                "  call void @mule_release(ptr %result)",
                f"  br i1 %bad.result, label %failed, label %success, {vops.COLD_BRANCH}",
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


def native_callbacks(module: ModuleGenerator, layout) -> str:
    """Adapt borrowed primitive ABI arguments to owned compiled Mule entries."""
    initializer = layout.methods["__init__"]
    count = len(initializer.vars) - 1
    constructor = [
        _function_comment(module.code(initializer), "native constructor callback"),
        f"define internal ptr {layout.constructor}(ptr %globals, ptr %class, i64 %count, ptr %args) {{",
        "entry:",
        f"  %wrong = icmp ne i64 %count, {count}",
        f"  br i1 %wrong, label %error, label %checked, {vops.COLD_BRANCH}",
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
            "  call i32 @mule_native_type_error() cold noinline",
            "  ret ptr null",
            "}",
        ]
    )
    if (
        initializer.signature.default_parameters
        or not initializer.signature.positional_body
    ):
        constructor = [
            _function_comment(module.code(initializer), "native constructor callback"),
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
        _function_comment(
            module.code(layout.protocols["get_field"]), "native field getter callback"
        ),
        f"define internal ptr {layout.getter}(ptr %globals, ptr %instance, i64 %index) {{",
        "entry:",
        *index,
        f"  %result = call ptr {module.bodies[layout.protocols['get_field']]}(ptr %globals, ptr %owned.instance, ptr %index.value)",
        "  ret ptr %result",
        "}",
    ]
    setter = [
        _function_comment(
            module.code(layout.protocols["set_field"]), "native field setter callback"
        ),
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
    "ForeignFunction",
    "Function",
    "FunctionGenerator",
    "ModuleGenerator",
    "Program",
    "_ConstantKey",
    "_needs_cleanup_release",
    "compile_component",
    "generate",
    "unit_finalizer",
    "unit_initializer",
]
