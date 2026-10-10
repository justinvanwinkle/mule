"""Compilation driver, following ``src/compiler/main.lisp``.

``compile_toplevel`` converts one module into IR1: the top-level lambda plus
one lambda per function, each in its own component once the initial component
is split by depth-first ordering. Later phases (optimization, local calls,
environment analysis, IR2) attach here as they are implemented.
"""

from __future__ import annotations

from dataclasses import dataclass

from mule.parser import parse

from .debug import check_ir1_consistency
from .dfo import find_initial_dfo
from .ir1tran import (
    Converter,
    _AnnotationShadows,
    _GlobalAnnotationStores,
    _ImportBindings,
)
from .ir1tran_lambda import convert_module, lambda_blocks
from .node import Component, Lambda


@dataclass(slots=True)
class Compilation:
    lambdas: list[Lambda]
    entry: Lambda
    components: list[Component]
    converter: Converter


def compile_toplevel(
    source: str,
    filename: str = "<unknown>",
    *,
    imports=None,
    prefix="",
    namespace="__main__",
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
    return Compilation(converter.functions, entry, components, converter)
