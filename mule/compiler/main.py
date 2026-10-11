"""Compilation driver, following ``src/compiler/main.lisp``.

``compile_toplevel`` converts one module into IR1: the top-level lambda plus
one lambda per function, each in its own component once the initial component
is split by depth-first ordering. Later phases (optimization, local calls,
environment analysis, IR2) attach here as they are implemented.
"""

from __future__ import annotations

from dataclasses import dataclass

from mule.parser import parse

from . import checkgen, ir1opt, locall
from .debug import check_ir1_consistency
from .dfo import find_dfo, find_initial_dfo
from .envanal import environment_analyze
from .ir1tran import (
    Converter,
    _AnnotationShadows,
    _GlobalAnnotationStores,
    _ImportBindings,
)
from .ir1tran_lambda import convert_module, lambda_blocks
from .ir1util import join_blocks_if_possible
from .node import Component, Lambda


@dataclass(slots=True)
class Compilation:
    lambdas: list[Lambda]
    entry: Lambda
    components: list[Component]
    converter: Converter
    facts: dict[Lambda, locall.LambdaFacts] | None = None

    @property
    def converters(self):
        return self.converter.function_converters


REOPTIMIZE_LIMIT = 10


def ir1_phases(compilation: Compilation) -> None:
    """Run the IR1 phases over a module's lambdas (``ir1-phases``).

    Simplification, local-call analysis, and flow propagation interact: a
    rewrite can expose a constant branch, and settled facts decide which casts
    become checks. Check generation can introduce code, so the phases repeat
    until no cast changes state.
    """
    while True:
        _ir1_optimize_phase_1(compilation)
        if not any(
            checkgen.generate_type_checks(lambda_, compilation.facts[lambda_].types)
            for lambda_ in compilation.lambdas
        ):
            break
    for component in compilation.components:
        join_blocks_if_possible(component)
        if component.reanalyze:
            find_dfo(component)
    check_ir1_consistency(compilation.components)


def _ir1_optimize_phase_1(compilation: Compilation) -> None:
    for _ in range(REOPTIMIZE_LIMIT):
        reoptimized = False
        for component in compilation.components:
            reoptimized |= ir1opt.ir1_optimize_until_done(component)
            if component.reanalyze:
                find_dfo(component)
        compilation.facts = locall.analyze(compilation.lambdas)
        if not reoptimized and not any(
            component.reoptimize for component in compilation.components
        ):
            return
    # The reoptimize limit was reached; the facts computed last remain valid.


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
    tree = parse(source, filename)
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
    entry = convert_module(converter, tree)
    for lambda_ in converter.functions:
        _GlobalAnnotationStores(converter, lambda_).apply(lambda_blocks(lambda_))
    components = find_initial_dfo(initial, converter.functions)
    check_ir1_consistency(components)
    compilation = Compilation(converter.functions, entry, components, converter)
    # SBCL runs environment analysis after the IR1 phases, once LET conversion
    # has settled the cleanup structure. Python's cleanup regions are fixed at
    # conversion, and the phases must see the cleanup code, so it runs first.
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
