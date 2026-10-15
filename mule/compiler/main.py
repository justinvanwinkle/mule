"""Compilation driver, following ``src/compiler/main.lisp``.

``compile_toplevel`` converts one module into IR1: the top-level lambda plus
one lambda per function, each in its own component once the initial component
is split by depth-first ordering. Later phases (optimization, local calls,
environment analysis, IR2) attach here as they are implemented.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field

from mule import ast
from mule.diagnostics import annotation_warning_level, phase
from mule.parser import parse

from . import checkgen, ir1opt, locall, loop
from .annotation_types import AnnotationLedger
from .debug import check_ir1_consistency
from .dfo import find_dfo, find_initial_dfo
from .envanal import environment_analyze
from .import_dependencies import discover as import_dependencies
from .interface_annotations import imported_bindings as interface_bindings
from .ir1tran import (
    BindsName,
    Converter,
    _AnnotationShadows,
    _GlobalAnnotationStores,
    _ImportBindings,
)
from .ir1tran_lambda import convert_module, lambda_blocks
from .ir1util import join_blocks_if_possible, substitute_lvar_uses, unlink_node
from .node import Cast, Component, Lambda, NodeVisitor, linked


@dataclass(slots=True)
class Compilation:
    lambdas: list[Lambda]
    entry: Lambda
    components: list[Component]
    converter: Converter
    facts: dict[Lambda, locall.LambdaFacts] | None = None
    annotations: AnnotationLedger = field(default_factory=AnnotationLedger)

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
        while True:
            _ir1_optimize_phase_1(compilation)
            if ignore_contradicted_annotations(compilation):
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
    rather than rejected. A typed native field's ``mule.int64`` declaration
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
        elif kind == "local":
            owner.local_annotations.pop(key[0], None)
            names = [var.name for var in owner.vars]
            if key[0] in names:
                annotations = list(owner.signature.annotations)
                annotations[names.index(key[0])] = None
                owner.signature.annotations = tuple(annotations)
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


def _ir1_optimize_phase_1(compilation: Compilation) -> None:
    for iteration in range(REOPTIMIZE_LIMIT):
        reoptimized = False
        with phase("IR1 optimization round %d", iteration + 1, level=logging.DEBUG):
            for component in compilation.components:
                reoptimized |= ir1opt.ir1_optimize_until_done(component)
                if component.reanalyze:
                    find_dfo(component)
        with phase("IR1 local-call and flow analysis", level=logging.DEBUG):
            compilation.facts = locall.analyze(
                compilation.lambdas, compilation.annotations
            )
        if not reoptimized and not any(
            component.reoptimize for component in compilation.components
        ):
            return
    # The reoptimize limit was reached; the facts computed last remain valid.
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
        converter.interface_bindings = interface_bindings(tree.statements)
        binds_name = BindsName("__name__")
        tree.accept(binds_name)
        if not binds_name.found:
            converter.module_name_literal = ast.TextLiteral(namespace)
        entry = convert_module(converter, tree)
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
        environment_analyze(compilation)
        for component in components:
            if component.reanalyze:
                find_dfo(component)
        check_ir1_consistency(components)
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
        ir1_phases(compilation)
    return compilation
