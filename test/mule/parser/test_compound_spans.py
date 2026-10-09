"""Compound spans end at their final suite statement, before dedent trivia."""

import pytest

from mule import ast

from .ast_query import at


@pytest.mark.parametrize(
    "source, path, start, end",
    [
        pytest.param(
            "if ready:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (2, 8),
            id="if-before-sibling",
        ),
        pytest.param(
            "if ready: pass; pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (1, 20),
            id="inline-if-multiple-statements",
        ),
        pytest.param(
            "if ready:\n    pass\nelse:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (4, 8),
            id="if-includes-else",
        ),
        pytest.param(
            "if ready:\n    pass\nelif other:\n    pass\nelse:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (6, 8),
            id="if-includes-elif-and-else",
        ),
        pytest.param(
            "if ready:\n    pass\nelif other:\n    pass\nelse:\n    pass\nnext_value = 1\n",
            "statements.0.else_body.0",
            (3, 0),
            (6, 8),
            id="elif-includes-own-else",
        ),
        pytest.param(
            "if first:\n    pass\nelif second:\n    pass\nelif third:\n    pass\nelse:\n    pass\nnext_value = 1\n",
            "statements.0.else_body.0",
            (3, 0),
            (8, 8),
            id="elif-includes-following-elif",
        ),
        pytest.param(
            "while ready:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (2, 8),
            id="while-before-sibling",
        ),
        pytest.param(
            "while ready:\n    pass\nelse:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (4, 8),
            id="while-includes-else",
        ),
        pytest.param(
            "for item in items:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (2, 8),
            id="for-before-sibling",
        ),
        pytest.param(
            "for item in items:\n    pass\nelse:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (4, 8),
            id="for-includes-else",
        ),
        pytest.param(
            "with manager:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (2, 8),
            id="with-before-sibling",
        ),
        pytest.param(
            "try:\n    pass\nexcept Error:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (4, 8),
            id="try-includes-handler",
        ),
        pytest.param(
            "try:\n    pass\nexcept Error:\n    pass\nelse:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (6, 8),
            id="try-includes-else",
        ),
        pytest.param(
            "try:\n    pass\nexcept Error:\n    pass\nelse:\n    pass\nfinally:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (8, 8),
            id="try-includes-finally",
        ),
        pytest.param(
            "try:\n    pass\nfinally:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (4, 8),
            id="try-finally-without-handlers",
        ),
        pytest.param(
            "try:\n    pass\nexcept Error:\n    pass\nexcept Other:\n    pass\nnext_value = 1\n",
            "statements.0.handlers.0",
            (3, 0),
            (4, 8),
            id="handler-excludes-next-handler",
        ),
        pytest.param(
            "try:\n    pass\nexcept* Error:\n    pass\nfinally:\n    pass\nnext_value = 1\n",
            "statements.0.handlers.0",
            (3, 0),
            (4, 8),
            id="group-handler-excludes-finally",
        ),
        pytest.param(
            "def function():\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (2, 8),
            id="function-before-sibling",
        ),
        pytest.param(
            "class Thing:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (2, 8),
            id="class-before-sibling",
        ),
        pytest.param(
            "@decorate\ndef function():\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (3, 8),
            id="decorated-function-start",
        ),
        pytest.param(
            "@decorate\nclass Thing:\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (3, 8),
            id="decorated-class-start",
        ),
        pytest.param(
            "@decorate\nasync def function():\n    pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (3, 8),
            id="decorated-async-function-start",
        ),
        pytest.param(
            "async def function():\n    async with manager:\n        pass\n    next_value = 1\n",
            "statements.0.body.0",
            (2, 4),
            (3, 12),
            id="async-with-before-sibling",
        ),
        pytest.param(
            "async def function():\n    async for item in items:\n        pass\n    next_value = 1\n",
            "statements.0.body.0",
            (2, 4),
            (3, 12),
            id="async-for-before-sibling",
        ),
        pytest.param(
            "def function():\n    if ready:\n        for item in items:\n            pass\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (4, 16),
            id="function-ending-in-nested-compounds",
        ),
        pytest.param(
            "def function():\n    if ready:\n        for item in items:\n            pass\nnext_value = 1\n",
            "statements.0.body.0",
            (2, 4),
            (4, 16),
            id="if-ending-in-nested-loop",
        ),
        pytest.param(
            "if ready:\n    π = 1\n\n# outside suite\nnext_value = 1\n",
            "statements.0",
            (1, 0),
            (2, 9),
            id="unicode-body-excludes-trailing-trivia",
        ),
    ],
)
def test_compound_span_covers_exact_syntactic_extent(parse, source, path, start, end):
    node = at(parse(source), path)
    assert node.span == ast.Span(ast.Position(*start), ast.Position(*end))
