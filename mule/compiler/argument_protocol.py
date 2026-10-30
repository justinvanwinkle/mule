"""Generate compiled external-entry binding rules for Python signatures."""

from mule import ast

from .inputs import read_source
from .ir1util import combinations
from .modules import STDLIB
from .node import CallKind, linked
from .protocols import add_lambdas, compile_protocol

# ``_arguments`` functions in definition order.
ROLES = ("construct", "bind")


def signature_literal(lambda_) -> ast.Tuple:
    """The constant tuple ``_arguments.bind`` reads a signature from."""
    signature = lambda_.signature
    kinds = signature.kinds
    positional_only = sum(kind == ast.ParameterKind.POSITIONAL_ONLY for kind in kinds)
    positional = sum(
        kind
        in (ast.ParameterKind.POSITIONAL_ONLY, ast.ParameterKind.POSITIONAL_OR_KEYWORD)
        for kind in kinds
    )
    rest = next(
        (
            index
            for index, kind in enumerate(kinds)
            if kind == ast.ParameterKind.VAR_POSITIONAL
        ),
        -1,
    )
    keywords = next(
        (
            index
            for index, kind in enumerate(kinds)
            if kind == ast.ParameterKind.VAR_KEYWORD
        ),
        -1,
    )
    offsets = {
        index: offset for offset, index in enumerate(signature.default_parameters)
    }
    return ast.Tuple(
        [
            ast.Tuple([ast.TextLiteral(name) for name in lambda_.parameters]),
            ast.IntegerLiteral(positional_only),
            ast.IntegerLiteral(positional),
            ast.IntegerLiteral(rest),
            ast.Tuple(
                [
                    ast.IntegerLiteral(offsets.get(index, -1))
                    for index in range(len(lambda_.parameters))
                ]
            ),
            ast.IntegerLiteral(keywords),
        ]
    )


def _keyword_calls(lambda_) -> bool:
    for call in combinations(lambda_):
        if (
            (
                call.kind in (CallKind.FULL, CallKind.GUARDED)
                or (
                    call.kind is CallKind.KNOWN
                    and linked(call.fun_info, call, "known function").name
                    == "%call_resolved"
                )
            )
            and call.info is not None
            and call.info.keywords
        ):
            return True
    return False


def bind_signatures(lambdas) -> None:
    """Record the signature ``_arguments.bind`` binds each function's calls
    with. A function with one takes keyword calls."""
    for lambda_ in lambdas:
        if (
            any(
                kind is not ast.ParameterKind.POSITIONAL_ONLY
                for kind in lambda_.signature.kinds
            )
            or lambda_.signature.default_parameters
        ):
            lambda_.binding = signature_literal(lambda_)


def prepare(program):
    """Compile ``_arguments`` when any function needs binding and record each
    signature. Binders enter through native wrappers, never through global
    name lookup. Returns the general constructor and the shared binder, or
    ``(None, None)`` when nothing binds."""
    if not any(lambda_.signature.kinds for lambda_ in program.lambdas) and not any(
        _keyword_calls(lambda_) for lambda_ in program.lambdas
    ):
        return None, None
    bind_signatures(program.lambdas)
    path = STDLIB / "_arguments.py"
    compilation = compile_protocol(
        read_source(path),
        "<compiled argument protocol>",
        prefix="mule_arguments_",
        namespace="$mule.arguments",
    )
    functions = compilation.lambdas[1:]
    for function, role in zip(functions, ROLES, strict=True):
        function.name = "mule_arguments_" + role
    add_lambdas(program, functions, compilation.facts)
    return functions
