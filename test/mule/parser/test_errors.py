"""Malformed grammar and tokenization with source-local Mule diagnostics.

Grammar: https://docs.python.org/3.14/reference/grammar.html
Semantic compiler errors, such as returning outside a function or duplicate
pattern captures, are deliberately excluded from these parser tests.
"""

import pytest

INVALID_CASES = [
    ("unexpected-indent", "    value = 1", "exec"),
    ("missing-suite", "if ready:\n", "exec"),
    ("unindented-suite", "if ready:\nrun()", "exec"),
    ("inconsistent-dedent", "if ready:\n    run()\n  stop()", "exec"),
    ("mixed-tab-space-indent", "if ready:\n\trun()\n        stop()", "exec"),
    ("missing-colon", "if ready\n    run()", "exec"),
    ("missing-condition", "if :\n    pass", "exec"),
    ("else-condition", "if ready:\n    pass\nelse other:\n    pass", "exec"),
    (
        "elif-after-else",
        "if ready:\n    pass\nelse:\n    pass\nelif other:\n    pass",
        "exec",
    ),
    ("try-without-handler", "try:\n    run()", "exec"),
    ("try-else-without-handler", "try:\n    run()\nelse:\n    pass", "exec"),
    (
        "mixed-except-and-star",
        "try:\n    run()\nexcept ValueError:\n    pass\nexcept* TypeError:\n    pass",
        "exec",
    ),
    ("bare-except-star", "try:\n    run()\nexcept*:\n    pass", "exec"),
    (
        "except-tuple-as-without-parens",
        "try:\n    run()\nexcept ValueError, TypeError as error:\n    pass",
        "exec",
    ),
    (
        "except-non-name-binding",
        "try:\n    run()\nexcept ValueError as errors[0]:\n    pass",
        "exec",
    ),
    ("for-missing-in", "for item values:\n    pass", "exec"),
    ("with-missing-item", "with :\n    pass", "exec"),
    ("decorator-without-definition", "@decorate\nvalue = 1", "exec"),
    ("unterminated-single-string", "'unfinished", "eval"),
    ("unterminated-triple-string", "'''unfinished", "eval"),
    ("newline-in-single-string", "'first\nsecond'", "eval"),
    ("invalid-string-prefix", "bf'bytes'", "eval"),
    ("mixed-string-bytes", "b'bytes' 'text'", "eval"),
    ("non-ascii-bytes", "b'café'", "eval"),
    ("invalid-hex-escape", "'\\xQ0'", "eval"),
    ("invalid-unicode-escape", "'\\u12xz'", "eval"),
    ("missing-hex-digits", "0x", "eval"),
    ("invalid-binary-digit", "0b102", "eval"),
    ("invalid-octal-digit", "0o89", "eval"),
    ("leading-zero-decimal", "0123", "eval"),
    ("double-number-underscore", "1__2", "eval"),
    ("trailing-number-underscore", "123_", "eval"),
    ("incomplete-exponent", "1e+", "eval"),
    ("identifier-after-number", "123abc", "eval"),
    ("unclosed-parenthesis", "(1 + 2", "eval"),
    ("mismatched-delimiters", "[1, 2)", "eval"),
    ("extra-close-delimiter", "1 + 2)", "eval"),
    ("missing-binary-right", "value +", "eval"),
    ("missing-unary-operand", "not", "eval"),
    ("missing-conditional-else", "one if condition", "eval"),
    ("missing-lambda-body", "lambda x:", "eval"),
    ("named-expression-without-parens", "value := 1", "eval"),
    ("named-expression-non-name-target", "(obj.attr := 1)", "eval"),
    ("call-positional-after-keyword", "f(name=1, 2)", "eval"),
    ("call-star-after-double-star", "f(**mapping, *values)", "eval"),
    ("call-bare-star", "f(*)", "eval"),
    ("generator-call-extra-argument", "f(x for x in values, 1)", "eval"),
    ("dict-comprehension-missing-value", "{x: for x in values}", "eval"),
    ("list-comprehension-missing-target", "[x for in values]", "eval"),
    ("list-comprehension-missing-iterable", "[x for x in]", "eval"),
    ("comprehension-else", "[x for x in values if flag else other]", "eval"),
    ("assignment-to-literal", "1 = value", "exec"),
    ("assignment-to-call", "f() = value", "exec"),
    ("augmented-tuple-target", "a, b += value", "exec"),
    ("annotated-tuple-target", "a, b: int = values", "exec"),
    ("delete-call", "del f()", "exec"),
    ("param-non-default-after-default", "def f(a=1, b):\n    pass", "exec"),
    ("param-slash-without-argument", "def f(/):\n    pass", "exec"),
    ("param-double-slash", "def f(a, /, b, /):\n    pass", "exec"),
    ("param-bare-star", "def f(*):\n    pass", "exec"),
    ("param-bare-star-only-kwargs", "def f(*, **kwargs):\n    pass", "exec"),
    ("param-vararg-default", "def f(*args=1):\n    pass", "exec"),
    ("param-kwargs-default", "def f(**kwargs=1):\n    pass", "exec"),
    ("param-after-kwargs", "def f(**kwargs, other):\n    pass", "exec"),
    ("lambda-annotation", "lambda x: int: x", "eval"),
    ("generic-empty-params", "type Box[] = int", "exec"),
    ("generic-bound-missing-expression", "type Box[T:] = int", "exec"),
    ("generic-default-missing-expression", "type Box[T =] = int", "exec"),
    ("alias-missing-value", "type Box[T] =", "exec"),
    ("fstring-empty-field", 'f"{}"', "eval"),
    ("fstring-invalid-conversion", 'f"{value!q}"', "eval"),
    ("fstring-unmatched-brace", 'f"literal }"', "eval"),
    ("template-and-plain-concatenation", 't"{value}" "suffix"', "eval"),
    (
        "pattern-invalid-arithmetic",
        "match value:\n    case 1 + 2:\n        pass",
        "exec",
    ),
    (
        "pattern-mapping-rest-wildcard",
        "match value:\n    case {**_}:\n        pass",
        "exec",
    ),
    ("pattern-bare-star", "match value:\n    case *rest:\n        pass", "exec"),
    (
        "pattern-sequence-double-star",
        "match value:\n    case [first, **rest]:\n        pass",
        "exec",
    ),
    (
        "pattern-mapping-bare-key",
        "match value:\n    case {name: item}:\n        pass",
        "exec",
    ),
    (
        "pattern-mapping-rest-nonfinal",
        'match value:\n    case {"a": first, **rest, "b": second}:\n        pass',
        "exec",
    ),
    (
        "pattern-class-positional-after-keyword",
        "match value:\n    case Point(x=first, second):\n        pass",
        "exec",
    ),
    (
        "pattern-class-star-argument",
        "match value:\n    case Point(*args):\n        pass",
        "exec",
    ),
    (
        "pattern-class-double-star-argument",
        "match value:\n    case Point(**kwargs):\n        pass",
        "exec",
    ),
    ("pattern-as-wildcard", "match value:\n    case item as _:\n        pass", "exec"),
    ("pattern-or-missing-branch", "match value:\n    case 1 |:\n        pass", "exec"),
    (
        "pattern-sequence-empty-slot",
        "match value:\n    case [first,, second]:\n        pass",
        "exec",
    ),
]


@pytest.mark.parametrize(
    "case,source,mode", INVALID_CASES, ids=[case for case, _, _ in INVALID_CASES]
)
def test_syntax_error_identifies_a_source_location(parse, case, source, mode):
    filename = "broken.py"
    with pytest.raises(SyntaxError) as actual:
        parse(source, filename=filename, mode=mode)
    error = actual.value
    assert error.filename == filename
    # A parser may point at EOF, including the empty line after a final newline.
    lines = source.split("\n")
    assert isinstance(error.lineno, int)
    assert 1 <= error.lineno <= len(lines)
    assert isinstance(error.offset, int)
    assert 1 <= error.offset <= len(lines[error.lineno - 1]) + 1
    if error.end_lineno is not None:
        assert error.lineno <= error.end_lineno <= len(lines)
        if error.end_offset is not None:
            assert 1 <= error.end_offset <= len(lines[error.end_lineno - 1]) + 1
