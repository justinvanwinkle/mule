"""Match suites end at their own body rather than a following dedent."""

from mule import ast
from test.mule.parser.ast_query import one


def test_match_and_each_case_end_before_following_siblings(parse):
    tree = parse(
        "match value:\n"
        "    case 1:\n"
        "        first()\n"
        "    case _:\n"
        "        final()\n"
        "after()\n"
    )
    match = one(tree, ast.Match)
    assert match.span == ast.Span(ast.Position(1, 0), ast.Position(5, 15))
    assert match.cases[0].span == ast.Span(ast.Position(2, 4), ast.Position(3, 15))
    assert match.cases[1].span == ast.Span(ast.Position(4, 4), ast.Position(5, 15))
    assert match.cases[0].pattern.span == ast.Span(
        ast.Position(2, 9), ast.Position(2, 10)
    )
    assert match.cases[1].pattern.span == ast.Span(
        ast.Position(4, 9), ast.Position(4, 10)
    )
    assert tree.statements[1].span == ast.Span(ast.Position(6, 0), ast.Position(6, 7))


def test_inline_case_span_includes_last_statement_and_excludes_newline(parse):
    match = one(
        parse("match value:\n    case _: first(); second()\nafter()\n"), ast.Match
    )
    assert match.span == ast.Span(ast.Position(1, 0), ast.Position(2, 29))
    assert match.cases[0].span == ast.Span(ast.Position(2, 4), ast.Position(2, 29))


def test_match_span_reaches_last_nested_body_token(parse):
    match = one(
        parse(
            "match value:\n"
            "    case _:\n"
            "        if ready:\n"
            "            finish()\n"
            "after()\n"
        ),
        ast.Match,
    )
    assert match.span == ast.Span(ast.Position(1, 0), ast.Position(4, 20))
    assert match.cases[0].span == ast.Span(ast.Position(2, 4), ast.Position(4, 20))
