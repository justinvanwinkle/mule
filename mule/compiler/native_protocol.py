"""Compile Mule class protocols and bind their physical storage primitives.

SBCL defstruct generates Lisp constructors and accessor forms. Here the
constructor and dynamic field adapters come from stdlib/_native_class.py and
pass through the same parser, IR1 phases, and code generator as user
functions. Only primitive operations have backend templates.
"""

from dataclasses import dataclass, field
from pathlib import Path

from .inputs import read_source
from .node import CallKind, Combination
from .protocols import add_lambdas, compile_protocol


@dataclass(slots=True)
class Layout:
    """A native class: its definition data, methods, and the compiled
    constructor and field adapters."""

    info: object
    methods: dict[str, object]
    index: int
    protocols: dict[str, object] = field(default_factory=dict)

    @property
    def schema(self):
        return f"@mule.layout.{self.index}"

    @property
    def constructor(self):
        return f"@mule.construct.{self.index}"

    @property
    def getter(self):
        return f"@mule.getfield.{self.index}"

    @property
    def setter(self):
        return f"@mule.setfield.{self.index}"


def layouts_of(lambdas) -> dict[str, Layout]:
    """Native class definitions anywhere in the program, by class identity."""
    layouts = {}
    for lambda_ in lambdas:
        definitions = {}
        for block in lambda_.component.blocks():
            for node in block.nodes():
                if not isinstance(node, Combination) or node.kind is not CallKind.KNOWN:
                    continue
                name = node.fun_info.name
                if name == "%make_function":
                    definitions[node] = node.args[0].use().leaf
                elif name == "%make_class" and node.info.class_id is not None:
                    info = node.info
                    methods = node.args[1:] if info.has_base else node.args
                    layouts[info.class_id] = Layout(
                        info,
                        {
                            method_name: definitions[value.use()]
                            for method_name, value in zip(
                                info.method_names, methods, strict=True
                            )
                        },
                        len(layouts),
                    )
    return layouts


def prepare(program):
    """Append compiled class protocols for every native class. Returns the
    layouts by class identity."""
    layouts = layouts_of(program.lambdas)
    if not layouts:
        return layouts
    path = Path(__file__).parents[2] / "stdlib/_native_class.py"
    template = read_source(path)
    for layout in layouts.values():
        parameters = ", ".join(
            f"p{i}" for i in range(len(layout.methods["__init__"].vars) - 1)
        )
        source = template.replace(
            ", PARAMETERS", ", " + parameters if parameters else ""
        )
        compilation = compile_protocol(
            source,
            str(path),
            namespace=f"$mule.native.{layout.index}",
            primitive_data={"_initialize": layout},
        )
        functions = compilation.lambdas[1:]
        for function, role in zip(
            functions, ("construct", "get_field", "set_field"), strict=True
        ):
            function.name = f"mule_protocol_{layout.index}_{role}"
            layout.protocols[role] = function
        add_lambdas(program, functions, compilation.facts)
    return layouts
