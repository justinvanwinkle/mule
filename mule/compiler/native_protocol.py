"""Compile Mule class protocols and bind their physical storage primitives.

SBCL defstruct generates Lisp constructors and accessor forms. Here the
constructor and dynamic field adapters come from stdlib/_native_class.py and
pass through the same parser, IR1 phases, and code generator as user
functions. Only primitive operations have backend templates.
"""

from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING

from .inputs import read_source
from .ir1util import class_method_definitions, known_calls
from .node import Lambda, linked
from .protocols import add_lambdas, compile_protocol

if TYPE_CHECKING:
    from .ir1tran import ClassInfo


@dataclass(slots=True)
class Layout:
    """A native class: its definition data, methods, and the compiled
    constructor and field adapters."""

    info: ClassInfo
    methods: dict[str, Lambda]
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
        methods = class_method_definitions(lambda_)
        for call in known_calls(lambda_, "%make_class"):
            info = linked(call.info, call, "class definition")
            if info.class_id is not None:
                layouts[info.class_id] = Layout(
                    info, methods.get(info.identity, {}), len(layouts)
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
            function.name = f"mule_protocol_{program.unit}_{layout.index}_{role}"
            layout.protocols[role] = function
        add_lambdas(program, functions, compilation.facts)
    return layouts
