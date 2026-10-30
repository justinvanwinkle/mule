"""Compilation driver, following ``src/compiler/main.lisp``.

``compile_toplevel`` converts one module into IR1: the top-level lambda plus
one lambda per function, each in its own component once the initial component
is split by depth-first ordering. Later phases (optimization, local calls,
environment analysis, IR2) attach here as they are implemented.
"""

from __future__ import annotations

import logging
from collections.abc import Mapping
from dataclasses import dataclass, field

from mule import ast
from mule.diagnostics import annotation_warning_level, phase
from mule.parser import parse

from . import (  # noqa: F401
    checkgen,
    containtran,
    formattran,
    ir1opt,
    locall,
    loop,
    modarith,
    seqtran,
    srctran,
    typetran,
)
from .annotation_types import AnnotationLedger
from .ctype import ValueType
from .debug import check_ir1_consistency
from .dfo import find_dfo, find_initial_dfo
from .envanal import environment_analyze
from .final_functions import FinalFunctions
from .import_dependencies import discover as import_dependencies
from .interface_annotations import imported_bindings as interface_bindings
from .ir1tran import (
    BindsName,
    Converter,
    _AnnotationShadows,
    _GlobalAnnotationStores,
    _ImportBindings,
    find_constant_leaf,
)
from .ir1tran_lambda import convert_module, lambda_blocks
from .ir1util import (
    join_blocks_if_possible,
    reoptimize_lvar,
    substitute_lvar_uses,
    unlink_node,
)
from .node import (
    Cast,
    Component,
    Constant,
    GlobalVar,
    Lambda,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Set,
    linked,
)
from .return_checks import assert_inferred_results


@dataclass(slots=True)
class Compilation:
    lambdas: list[Lambda]
    entry: Lambda
    components: list[Component]
    converter: Converter
    facts: dict[Lambda, locall.LambdaFacts] | None = None
    locall_state: dict | None = None
    annotations: AnnotationLedger = field(default_factory=AnnotationLedger)
    imported_function_types: Mapping[tuple[str, str], ValueType] | None = None
    imported_targets: Mapping[tuple[str, str], object] | None = None
    storage_methods: Mapping[tuple[str, str], ValueType] | None = None
    builtin_return_types: Mapping[str, ValueType] | None = None
    builtin_targets: Mapping[str, object] | None = None

    @property
    def converters(self):
        return self.converter.function_converters


REOPTIMIZE_LIMIT = 10
logger = logging.getLogger(__name__)


def ir1_phases(compilation: Compilation) -> None:
    """Run the IR1 phases over a module's lambdas (``ir1-phases``).

    Simplification, local-call analysis, and flow propagation interact: a
    rewrite can expose a constant branch, and settled facts decide which casts
    become checks. Check generation can introduce code, so the phases repeat
    until no cast changes state.
    """
    with phase("IR1 phases %s", compilation.converter.filename, level=logging.DEBUG):
        facts_stale = True
        while True:
            _ir1_optimize_phase_1(compilation, facts_stale=facts_stale)
            facts_stale = False
            if ignore_contradicted_annotations(compilation):
                facts_stale = True
                continue
            if assert_inferred_results(compilation):
                facts_stale = True
                continue
            facts = linked(compilation.facts, compilation, "facts")
            with phase("IR1 check generation", level=logging.DEBUG):
                changed = any(
                    checkgen.generate_type_checks(lambda_, facts[lambda_].types)
                    for lambda_ in compilation.lambdas
                )
            if not changed:
                break
        with phase("IR1 final graph cleanup", level=logging.DEBUG):
            for component in compilation.components:
                join_blocks_if_possible(component)
                find_dfo(component)
                loop.analyze(component)
            check_ir1_consistency(compilation.components)


def ignore_contradicted_annotations(compilation: Compilation) -> bool:
    """Warn about annotations analysis proved wrong and stop enforcing or
    trusting them: their declarations are cleared and every assertion of
    them is removed. Returns whether anything changed, so analysis reruns.

    Python never enforces annotations, so a provably wrong one is reported
    rather than rejected. A typed native field's ``mule_types.int64`` declaration
    defines its raw storage, so a contradiction there remains an error."""
    ledger = compilation.annotations
    pending = ledger.contradictions
    ledger.contradictions = []
    changed = False
    for source, location, name in pending:
        if source is None:
            raise SyntaxError(f"value contradicts annotation {name}", location)
        if source in ledger.ignored:
            continue
        ledger.ignored.add(source)
        changed = True
        filename, line, column = location[:3]
        logger.log(
            annotation_warning_level(filename),
            "%s:%s:%s: warning: value contradicts annotation %s; the annotation is ignored",
            filename,
            line,
            column,
            name,
        )
        kind, owner, *key = source
        if kind == "return":
            owner.signature.return_annotation = None
            owner.ir1_revision += 1
        elif kind == "local":
            owner.local_annotations.pop(key[0], None)
            names = [var.name for var in owner.vars]
            if key[0] in names:
                annotations = list(owner.signature.annotations)
                annotations[names.index(key[0])] = None
                owner.signature.annotations = tuple(annotations)
            owner.ir1_revision += 1
    if not changed:
        return False
    remover = _RemoveIgnoredCast(ledger)
    for component in compilation.components:
        for block in list(component.blocks()):
            for node in list(block.nodes()):
                node.accept(remover)
        component.reoptimize = True
    return True


class _RemoveIgnoredCast(NodeVisitor):
    def __init__(self, ledger: AnnotationLedger):
        self.ledger = ledger

    def visit_Cast(self, node):
        if node.declaration in self.ledger.ignored:
            _remove_cast(node)


def _remove_cast(node: Cast) -> None:
    """Retarget input producers to the merged result, as SBCL delete-filter."""
    value, result = linked(node.value, node, "asserted value"), node.lvar
    node.value = None
    value.dest = None
    substitute_lvar_uses(result, value)
    unlink_node(node)


class _ConstantLeaf(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Cast(self, node: Cast):
        return node.value.uses[0].accept(self) if len(node.value.uses) == 1 else None

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf: Constant):
        return leaf


_CONSTANT_LEAF = _ConstantLeaf()


class _GlobalConstantScan(NodeVisitor, LeafVisitor):
    def __init__(
        self,
        toplevel: bool,
        imported_types: Mapping[tuple[str, str], ValueType] | None = None,
        imported_targets: Mapping[tuple[str, str], object] | None = None,
        constants_table: dict | None = None,
        allowed_stores: set[Set] | None = None,
        allowed_names: set[str] | None = None,
    ):
        self.toplevel = toplevel
        self.imported_types = imported_types
        self.imported_targets = imported_targets
        self.constants_table = constants_table
        self.allowed_stores = allowed_stores
        self.allowed_names = allowed_names
        self.counts: dict[str, int] = {}
        self.constants: dict[str, Constant] = {}
        self._store: Set | None = None

    def visit_Set(self, node: Set):
        self._store = node
        node.var.accept(self)
        self._store = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        self.counts[leaf.name] = self.counts.get(leaf.name, 0) + 1
        store = self._store
        if self.toplevel and store is not None and store.value is not None:
            if self.allowed_stores is not None and store not in self.allowed_stores:
                return
            constant = None
            if self.allowed_names is None or leaf.name in self.allowed_names:
                constant = store.value.use().accept(_CONSTANT_LEAF)
            if (
                constant is None
                and self.imported_types is not None
                and self.constants_table is not None
                and len(store.value.uses) == 1
            ):
                origin = store.value.uses[0].accept(ir1opt._ImportedAttrOrigin())
                if origin is not None and (
                    self.imported_targets is None or origin not in self.imported_targets
                ):
                    vtype = self.imported_types.get(origin)
                    if vtype is not None:
                        literal = vtype.accept(ir1opt._SINGLETON_LITERAL)
                        if literal is not None:
                            constant = find_constant_leaf(self.constants_table, literal)
            if constant is not None:
                self.constants[leaf.name] = constant


class _ReplaceGlobalConstantRefs(NodeVisitor, LeafVisitor):
    def __init__(self, constants: dict[str, Constant]):
        self.constants = constants
        self.changed = False
        self._ref: Ref | None = None

    def visit_Ref(self, node: Ref):
        self._ref = node
        node.leaf.accept(self)
        self._ref = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        constant = self.constants.get(leaf.name)
        ref = self._ref
        if constant is None or ref is None:
            return
        if ref.lexenv.policy.safety >= 3 or (
            ref.home_lambda is not None and ref.home_lambda.policy.safety >= 3
        ):
            return
        if ref in leaf.refs:
            leaf.refs.remove(ref)
        ref.leaf = constant
        ref.derived_type = ir1opt.constant_type(constant.value)
        constant.refs.append(ref)
        if ref.lvar is not None:
            reoptimize_lvar(ref.lvar)
        if (home := ref.home_lambda) is not None:
            home.ir1_revision += 1
        self.changed = True


def _fold_protocol_constants(compilation: Compilation) -> bool:
    is_protocol = compilation.entry.is_protocol
    allowed_names: set[str] | None = None
    allowed_stores: set[Set] | None = None
    if not is_protocol:
        if compilation.entry.policy.safety >= 3:
            return False
        allowed_names = {
            name
            for name, expr in compilation.entry.annotation_expressions.items()
            if expr.accept(locall._IS_FINAL_ANNOTATION)
        }
        if (
            not allowed_names and not compilation.imported_function_types
        ) or locall.has_module_namespace_mutations(compilation.lambdas):
            return False
        allowed_stores = locall._unconditional_toplevel_stores(compilation.entry)
    counts: dict[str, int] = {}
    constants: dict[str, Constant] = {}
    entry_constants = (
        compilation.entry.component.constants
        if compilation.entry.component is not None
        else None
    )
    for lambda_ in compilation.lambdas:
        if lambda_.component is None:
            continue
        scan = _GlobalConstantScan(
            lambda_ is compilation.entry,
            imported_types=compilation.imported_function_types,
            imported_targets=compilation.imported_targets,
            constants_table=entry_constants,
            allowed_stores=allowed_stores,
            allowed_names=allowed_names,
        )
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(scan)
        for name, count in scan.counts.items():
            counts[name] = counts.get(name, 0) + count
        constants.update(scan.constants)
    single_constants = {
        name: constant for name, constant in constants.items() if counts.get(name) == 1
    }
    if not single_constants:
        return False
    changed = False
    for lambda_ in compilation.lambdas:
        if lambda_ is compilation.entry or lambda_.component is None:
            continue
        replacer = _ReplaceGlobalConstantRefs(single_constants)
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(replacer)
        if replacer.changed:
            lambda_.component.reoptimize = True
            changed = True
    return changed


def _ir1_optimize_phase_1(
    compilation: Compilation, *, facts_stale: bool = True
) -> None:
    if compilation.locall_state is None:
        compilation.locall_state = {}
    for iteration in range(REOPTIMIZE_LIMIT):
        ir1_changed = False
        with phase("IR1 optimization round %d", iteration + 1, level=logging.DEBUG):
            for component in compilation.components:
                ir1_changed |= ir1opt.ir1_optimize_until_done(component)
                if component.reanalyze:
                    find_dfo(component)
                    ir1_changed = True
        if ir1_changed:
            facts_stale = True
        if _fold_protocol_constants(compilation):
            facts_stale = True
            continue
        if any(component.reoptimize for component in compilation.components):
            continue
        if compilation.facts is None or facts_stale:
            with phase("IR1 local-call and flow analysis", level=logging.DEBUG):
                compilation.facts = locall.analyze(
                    compilation.lambdas,
                    compilation.annotations,
                    imported_function_types=compilation.imported_function_types,
                    imported_targets=compilation.imported_targets,
                    storage_methods=compilation.storage_methods,
                    builtin_return_types=compilation.builtin_return_types,
                    builtin_targets=compilation.builtin_targets,
                    state_cache=compilation.locall_state,
                )
                facts_stale = False
            if any(component.reoptimize for component in compilation.components):
                continue
        hoisted = False
        for component in compilation.components:
            hoisted |= loop.hoist_loop_invariants(component)
        if not hoisted:
            return
        facts_stale = True
    if facts_stale:
        with phase("IR1 local-call and flow analysis", level=logging.DEBUG):
            compilation.facts = locall.analyze(
                compilation.lambdas,
                compilation.annotations,
                imported_function_types=compilation.imported_function_types,
                imported_targets=compilation.imported_targets,
                storage_methods=compilation.storage_methods,
                builtin_return_types=compilation.builtin_return_types,
                builtin_targets=compilation.builtin_targets,
                state_cache=compilation.locall_state,
            )
    logger.debug("IR1 reoptimization reached its %d-round limit", REOPTIMIZE_LIMIT)


class _ParsedModule(ast.Visitor):
    """Whole-file parsing must produce a module."""

    def __init__(self, filename: str):
        self.filename = filename

    def visit_Node(self, node: ast.Node):
        raise TypeError(f"parsing {self.filename} produced {node!r}, not a module")

    def visit_Module(self, node: ast.Module) -> ast.Module:
        return node


def compile_toplevel(
    source: str,
    filename: str = "<unknown>",
    *,
    imports=None,
    prefix="",
    namespace="__main__",
    primitives=None,
    primitive_data=None,
) -> Compilation:
    with phase("parse %s", filename, level=logging.DEBUG):
        tree = parse(source, filename).accept(_ParsedModule(filename))
    if imports is not None:
        from pathlib import Path

        from .modules import is_library_path

        if not is_library_path(Path(filename)):
            imports.include_patterns(import_dependencies(tree))
    with phase("IR1 conversion %s", filename, level=logging.DEBUG):
        initial = Component("initial")
        converter = Converter(
            source,
            filename,
            imports=imports,
            prefix=prefix,
            namespace=namespace,
            component=initial,
            primitives=primitives,
            primitive_data=primitive_data,
        )
        bindings = _ImportBindings(converter)
        for statement in tree.statements:
            statement.accept(bindings)
        shadows = _AnnotationShadows()
        for statement in tree.statements:
            statement.accept(shadows)
        converter.module_annotation_shadows = shadows.names
        converter.module_binding_counts = shadows.counts
        converter.final_functions = FinalFunctions.scan(tree.statements, shadows.counts)
        shadows.register_declared_classes(converter, tree.statements)
        converter.interface_bindings = interface_bindings(tree.statements)
        binds_name = BindsName("__name__")
        tree.accept(binds_name)
        if not binds_name.found:
            converter.module_name_literal = ast.TextLiteral(namespace)
        if namespace == "__main__" and not filename.startswith("<"):
            binds_file = BindsName("__file__")
            tree.accept(binds_file)
            if not binds_file.found:
                converter.module_file_literal = ast.TextLiteral(filename)
        entry = convert_module(converter, tree)
        is_protocol = primitives is not None or namespace.startswith("$")
        for lambda_ in converter.functions:
            lambda_.is_protocol = is_protocol
        if converter.module_annotations:
            for lambda_ in converter.functions:
                _GlobalAnnotationStores(converter, lambda_).apply(
                    lambda_blocks(lambda_)
                )
        components = find_initial_dfo(initial, converter.functions)
        converter.split = True
        check_ir1_consistency(components)
        compilation = Compilation(
            list(converter.functions), entry, components, converter
        )
        # Unit preparation simplifies IR1 before the full IR1 phases. Publish
        # namespace mutation before either optimizer can recognize builtins.
        if locall.has_module_namespace_mutations(compilation.lambdas):
            for component in compilation.components:
                component.mutates_module_namespace = True
    logger.debug(
        "IR1 module %s: %d lambdas, %d components",
        filename,
        len(compilation.lambdas),
        len(compilation.components),
    )
    # SBCL runs environment analysis after the IR1 phases, once LET conversion
    # has settled the cleanup structure. Python's cleanup regions are fixed at
    # conversion, and the phases must see the cleanup code, so it runs first.
    with phase("environment analysis %s", filename, level=logging.DEBUG):
        initial_component_count = len(compilation.components)
        environment_analyze(compilation)
        reanalyzed = len(compilation.components) != initial_component_count
        for component in compilation.components:
            if component.reanalyze:
                find_dfo(component)
                reanalyzed = True
        if reanalyzed:
            check_ir1_consistency(compilation.components)
    return compilation


def compile_source(
    source: str,
    filename: str = "<unknown>",
    *,
    imports=None,
    prefix="",
    namespace="__main__",
    primitives=None,
) -> Compilation:
    """Convert one module and run the IR1 phases over it."""
    with phase("compile source %s", filename):
        compilation = compile_toplevel(
            source,
            filename,
            imports=imports,
            prefix=prefix,
            namespace=namespace,
            primitives=primitives,
        )
        if imports is not None:
            compilation.imported_function_types = imports.exported_types
            compilation.imported_targets = imports.exported_targets
            compilation.storage_methods = imports.storage_methods
            compilation.builtin_return_types = imports.builtin_return_types
            compilation.builtin_targets = imports.builtin_targets
        elif not compilation.entry.is_protocol:
            from .driver import cached_builtins_interface

            interface = cached_builtins_interface()
            compilation.imported_function_types = interface.exported_types
            compilation.imported_targets = interface.exported_targets
            compilation.storage_methods = interface.storage_methods
            compilation.builtin_return_types = interface.builtin_return_types
            compilation.builtin_targets = interface.builtin_targets
        ir1_phases(compilation)
    return compilation
