"""Generate compiled external-entry binding rules for Python signatures."""

from pathlib import Path

from mule import ast

from .node import CallKind, Combination
from .protocols import add_lambdas, compile_protocol


def binder_source(lambda_):
    signature = lambda_.signature
    kinds = signature.kinds
    parameters = lambda_.parameters
    positional = [
        index
        for index, kind in enumerate(kinds)
        if kind
        in (ast.ParameterKind.POSITIONAL_ONLY, ast.ParameterKind.POSITIONAL_OR_KEYWORD)
    ]
    rest = next(
        (
            index
            for index, kind in enumerate(kinds)
            if kind == ast.ParameterKind.VAR_POSITIONAL
        ),
        None,
    )
    defaults = {
        index: offset for offset, index in enumerate(signature.default_parameters)
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
    for index in range(len(parameters)):
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
        for index, kind in enumerate(kinds)
        if kind
        in (ast.ParameterKind.POSITIONAL_OR_KEYWORD, ast.ParameterKind.KEYWORD_ONLY)
    ]
    for branch, index in enumerate(accepted):
        name = parameters[index]
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
    for index, name in enumerate(parameters):
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
    values = ", ".join(f"p{index}" for index in range(len(parameters)))
    lines.append(
        f"    return _binding_body(body_globals, view{', ' + values if values else ''})"
    )
    return "\n".join(lines) + "\n"


def _keyword_calls(lambda_) -> bool:
    for block in lambda_.component.blocks():
        for node in block.nodes():
            if (
                isinstance(node, Combination)
                and (
                    node.kind in (CallKind.FULL, CallKind.GUARDED)
                    or (
                        node.kind is CallKind.KNOWN
                        and node.fun_info.name == "%call_resolved"
                    )
                )
                and node.info is not None
                and node.info.keywords
            ):
                return True
    return False


def prepare(program):
    """Binders enter through native wrappers, never through global name
    lookup. Returns the general constructor lambda, or ``None`` when no
    function needs binding."""
    if not any(lambda_.signature.kinds for lambda_ in program.lambdas) and not any(
        _keyword_calls(lambda_) for lambda_ in program.lambdas
    ):
        return None

    def append(source, prefix, body=None):
        compilation = compile_protocol(
            source,
            "<compiled argument protocol>",
            prefix=prefix,
            namespace="$mule.arguments",
            primitive_data={"_binding_body": body} if body is not None else None,
        )
        functions = compilation.lambdas[1:]
        add_lambdas(program, functions, compilation.facts)
        return functions[0]

    for index, lambda_ in enumerate(tuple(program.lambdas)):
        if lambda_.signature.kinds:
            binder = append(binder_source(lambda_), f"mule_arguments_{index}_", lambda_)
            lambda_.binding = binder.name
    path = Path(__file__).resolve().parents[2] / "stdlib/_arguments.py"
    return append(path.read_text(), "mule_arguments_construct_")
