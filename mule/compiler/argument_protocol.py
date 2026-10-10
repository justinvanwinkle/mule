"""Generate compiled external-entry binding rules for Python signatures."""

from pathlib import Path

from mule import ast

from . import ir
from .format_protocol import LookupMethod, TextAccess
from .frontend import lower
from .protocols import Bindings, Primitive


def binder_source(function):
    positional = [
        index
        for index, kind in enumerate(function.parameter_kinds)
        if kind
        in (ast.ParameterKind.POSITIONAL_ONLY, ast.ParameterKind.POSITIONAL_OR_KEYWORD)
    ]
    rest = next(
        (
            index
            for index, kind in enumerate(function.parameter_kinds)
            if kind == ast.ParameterKind.VAR_POSITIONAL
        ),
        None,
    )
    defaults = {
        index: offset for offset, index in enumerate(function.default_parameters)
    }
    lines = [
        "def bind(view, body_globals):",
        "    missing = _binding_missing()",
        "    count = _binding_count(view)",
    ]
    if rest is None:
        lines += [
            f"    if count > {len(positional)}:",
            "        raise TypeError('too many positional arguments')",
        ]
    for index in range(len(function.parameters)):
        if index == rest:
            lines.append(f"    p{index} = _binding_rest(view, {len(positional)})")
        elif index in positional:
            offset = positional.index(index)
            lines += [
                f"    if count > {offset}:",
                f"        p{index} = _binding_pos(view, {offset})",
                "    else:",
                f"        p{index} = missing",
            ]
        else:
            lines.append(f"    p{index} = missing")
    lines += [
        "    index = 0",
        "    keywords = _binding_keyword_count(view)",
        "    while index < keywords:",
        "        name = _binding_keyword_name(view, index)",
    ]
    accepted = [
        index
        for index, kind in enumerate(function.parameter_kinds)
        if kind
        in (ast.ParameterKind.POSITIONAL_OR_KEYWORD, ast.ParameterKind.KEYWORD_ONLY)
    ]
    for branch, index in enumerate(accepted):
        name = function.parameters[index]
        lines += [
            f"        {'if' if branch == 0 else 'elif'} name == {name!r}:",
            f"            if p{index} is not missing:",
            f"                raise TypeError({('multiple values for argument ' + name)!r})",
            f"            p{index} = _binding_keyword_value(view, index)",
        ]
    lines += [
        "        else:" if accepted else "        if True:",
        "            raise TypeError('unexpected keyword argument')",
        "        index += 1",
    ]
    for index, name in enumerate(function.parameters):
        if index == rest:
            continue
        lines.append(f"    if p{index} is missing:")
        if index in defaults:
            lines.append(
                f"        p{index} = _binding_default(view, {defaults[index]})"
            )
        else:
            lines.append(
                f"        raise TypeError({('missing required argument ' + name)!r})"
            )
    values = ", ".join(f"p{index}" for index in range(len(function.parameters)))
    lines.append(
        f"    return _binding_body(body_globals{', ' + values if values else ''})"
    )
    return "\n".join(lines) + "\n"


def prepare(module):
    """Binders enter through native wrappers, never through global name lookup."""
    calls, references = {}, set()
    if not any(function.parameter_kinds for function in module.functions) and not any(
        operation.accept(KeywordCalls())
        for function in module.functions
        for block in function.blocks
        for operation in block.instructions
    ):
        return None, calls, references
    primitives = {
        name: TextAccess("mule" + name)
        for name in (
            "_binding_missing",
            "_binding_count",
            "_binding_pos",
            "_binding_rest",
            "_binding_keyword_count",
            "_binding_keyword_name",
            "_binding_keyword_value",
            "_binding_default",
            "_allocate_instance",
            "_forward_call",
        )
    }
    primitives["_init_method"] = LookupMethod("__init__")

    def append(source, prefix, body=None):
        lowered = lower(
            source,
            "<compiled argument protocol>",
            prefix=prefix,
            namespace="$mule.arguments",
        )
        functions = lowered.functions[1:]
        for function in functions:
            bindings = dict(primitives)
            if body is not None:
                bindings["_binding_body"] = BoundBody(body)
            collector = Bindings(bindings)
            for block in function.blocks:
                for operation in block.instructions:
                    operation.accept(collector)
            calls.update(
                ((function.name, result), primitive)
                for result, primitive in collector.calls.items()
            )
            references.update(
                (function.name, result) for result in collector.references
            )
        module.functions.extend(functions)
        return functions[0].name

    for index, function in enumerate(tuple(module.functions)):
        if function.parameter_kinds:
            function.binding = append(
                binder_source(function), f"mule_arguments_{index}_", function.name
            )
    path = Path(__file__).resolve().parents[2] / "stdlib/_arguments.py"
    constructor = append(path.read_text(), "mule_arguments_construct_")
    return constructor, calls, references


class KeywordCalls(ir.Visitor):
    def visit_Call(self, operation):
        return bool(operation.keywords)

    def visit_CallMethod(self, operation):
        return bool(operation.keywords)


class BoundBody(Primitive):
    """The external entry's final local call shares the caller's logical frame."""

    def __init__(self, function):
        self.function = function

    def emit(self, emitter, arguments):
        owned = [
            emitter.operation(f"call ptr @mule.fast.retain(ptr {value})")
            for value in arguments[1:]
        ]
        operands = ", ".join(
            [f"ptr {arguments[0]}", *(f"ptr {value}" for value in owned)]
        )
        return emitter.operation(
            f"call ptr {emitter.module.bodies[self.function]}({operands})"
        )
