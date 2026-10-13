"""Compare the compiled-language tokenizer with Python 3.14's tokenizer."""

import importlib
import importlib.util
import io
import tokenize as reference_public_tokenize
from pathlib import Path

import pytest

from test.mule.compiler.test_native import run_native

reference_tokenize = importlib.import_module("_tokenize")


def _load_source(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def mule_tokenizer():
    root = Path(__file__).resolve().parents[3]
    # Load the independent stdlib implementation without replacing the real
    # extension module used as the oracle.
    return _load_source(root / "stdlib/_tokenize.py", "mule_test_tokenizer")


SOURCES = [
    pytest.param("", id="empty"),
    pytest.param("x = 1 # comment\n\n", id="comments-and-blank-lines"),
    pytest.param("# comment", id="comment-without-newline"),
    pytest.param(
        "π = 'λ'\nidentifier\u0301 = 2\n", id="unicode-columns-and-identifiers"
    ),
    pytest.param(
        "0xff <= 255; 0b10 != 2; .5 + 1e-3 + 2.5j\n", id="numbers-and-exact-operators"
    ),
    pytest.param(
        "if value:\n    x = 1\n    if x:\n        y = 2\nz = 3\n",
        id="nested-indentation",
    ),
    pytest.param("if value:\n\tx = 1\n", id="tab-indentation-and-eof-dedent"),
    pytest.param("if value:\n\f    x = 1\n", id="formfeed-indentation"),
    pytest.param("x = 1", id="missing-final-newline"),
    pytest.param("x = 1\n    ", id="trailing-whitespace-line"),
    pytest.param("x = 1\r\ny = 2\r\n", id="crlf"),
    pytest.param("x = (1\n     + 2)\n", id="bracket-continuation"),
    pytest.param("x = 1 + \\\n    2\n", id="explicit-continuation"),
    pytest.param("x = r'\\path'; y = b'bytes'\n", id="ordinary-string-prefixes"),
    pytest.param('x = """first\nsecond"""\n', id="multiline-string"),
    pytest.param('f"hello {name}"\n', id="fstring"),
    pytest.param(
        "f\"{{literal}} {mapping['key']}\"\n", id="escaped-braces-and-nested-quotes"
    ),
    pytest.param('f"{value!r:>{width}}"\n', id="conversion-and-nested-format-field"),
    pytest.param("f\"{f'{value}'}\"\n", id="nested-fstring"),
    pytest.param('f"{(\n    1 + 2\n)}"\n', id="multiline-replacement-field"),
    pytest.param('f"""first {value}\nlast"""\n', id="multiline-fstring"),
    pytest.param('t"hello {name!s}"\n', id="template-string"),
]


@pytest.mark.parametrize("extra_tokens", [False, True])
@pytest.mark.parametrize("source", SOURCES)
def test_tokenizer_tokens_match_cpython(mule_tokenizer, source, extra_tokens):
    expected = list(
        reference_tokenize.TokenizerIter(
            io.StringIO(source).readline, extra_tokens=extra_tokens
        )
    )
    actual = list(
        mule_tokenizer.TokenizerIter(
            io.StringIO(source).readline, extra_tokens=extra_tokens
        )
    )
    assert actual == expected


@pytest.mark.parametrize("extra_tokens", [False, True])
@pytest.mark.parametrize("encoding", ["utf-8", "latin-1", "utf-16"])
def test_tokenizer_explicit_encoding_matches_cpython(
    mule_tokenizer, encoding, extra_tokens
):
    # Each callback supplies a complete encoded physical line. In particular,
    # BytesIO.readline would split UTF-16 code units at a raw 0x0a byte.
    chunks = [line.encode(encoding) for line in ["é = 1\n", "x = 'é'"]]
    expected = list(
        reference_tokenize.TokenizerIter(
            iter(chunks).__next__, encoding=encoding, extra_tokens=extra_tokens
        )
    )
    actual = list(
        mule_tokenizer.TokenizerIter(
            iter(chunks).__next__, encoding=encoding, extra_tokens=extra_tokens
        )
    )
    assert actual == expected


def test_tokenizer_stopiteration_callback_is_eof(mule_tokenizer):
    lines = ["x = 1\n", "y = 2"]
    expected = list(
        reference_tokenize.TokenizerIter(iter(lines).__next__, extra_tokens=True)
    )
    actual = list(mule_tokenizer.TokenizerIter(iter(lines).__next__, extra_tokens=True))
    assert actual == expected


class _Reader:
    def __init__(self, lines):
        self.lines = iter(lines)
        self.read_lines = []

    def __call__(self):
        line = next(self.lines, "")
        self.read_lines.append(line)
        return line


def test_tokenizer_reads_only_the_line_needed_for_next_token(mule_tokenizer):
    reader = _Reader(["first = 1\n", "second = 2\n"])
    tokens = mule_tokenizer.TokenizerIter(reader, extra_tokens=True)
    assert reader.read_lines == []
    assert next(tokens)[1] == "first"
    assert reader.read_lines == ["first = 1\n"]
    # The remaining tokens of the first line must not consume the second line.
    assert [next(tokens)[1] for _ in range(3)] == ["=", "1", "\n"]
    assert reader.read_lines == ["first = 1\n"]
    assert next(tokens)[1] == "second"
    assert reader.read_lines == ["first = 1\n", "second = 2\n"]


def test_tokenizer_callback_failure_propagates(mule_tokenizer):
    def failing_reader():
        raise OSError("source cannot be read")

    tokens = mule_tokenizer.TokenizerIter(failing_reader, extra_tokens=True)
    with pytest.raises(OSError, match="source cannot be read"):
        next(tokens)


@pytest.mark.parametrize(
    "value,encoding",
    [(None, None), (b"x = 1\n", None), ("x = 1\n", "utf-8")],
)
def test_tokenizer_wrong_callback_result_type_raises(mule_tokenizer, value, encoding):
    def reader():
        return value

    options = {"extra_tokens": True}
    if encoding is not None:
        options["encoding"] = encoding
    with pytest.raises(TypeError):
        next(mule_tokenizer.TokenizerIter(reader, **options))


def test_tokenizer_noncallable_source_fails_on_iteration(mule_tokenizer):
    tokens = mule_tokenizer.TokenizerIter("x = 1", extra_tokens=True)
    with pytest.raises(TypeError):
        next(tokens)


def test_tokenizer_extra_tokens_argument_is_required(mule_tokenizer):
    with pytest.raises(TypeError):
        mule_tokenizer.TokenizerIter(io.StringIO("x = 1\n").readline)


def test_tokenizer_remains_exhausted_without_reading_again(mule_tokenizer):
    reader = _Reader(["x = 1\n"])
    tokens = mule_tokenizer.TokenizerIter(reader, extra_tokens=True)
    assert iter(tokens) is tokens
    list(tokens)
    consumed = list(reader.read_lines)
    for _ in range(2):
        with pytest.raises(StopIteration):
            next(tokens)
    assert reader.read_lines == consumed


ERROR_SOURCES = [
    'x = "unterminated\n',
    'x = """unterminated\n',
    "x = 0x_\n",
    "x = 1_\n",
    "x = \\oops\n",
    "x = \x00\n",
    "x = (\n",
    "if value:\n    pass\n  x = 1\n",
    "if value:\n\tpass\n        pass\n",
    'f"unmatched }"\n',
    'f"{value"\n',
]


@pytest.mark.parametrize("extra_tokens", [False, True])
@pytest.mark.parametrize("source", ERROR_SOURCES)
def test_tokenizer_lexical_errors_have_cpython_category_and_location(
    mule_tokenizer, source, extra_tokens
):
    with pytest.raises(SyntaxError) as expected:
        list(
            reference_tokenize.TokenizerIter(
                io.StringIO(source).readline, extra_tokens=extra_tokens
            )
        )
    with pytest.raises(type(expected.value)) as actual:
        list(
            mule_tokenizer.TokenizerIter(
                io.StringIO(source).readline, extra_tokens=extra_tokens
            )
        )
    assert type(actual.value) is type(expected.value)
    assert actual.value.lineno == expected.value.lineno
    assert actual.value.offset == expected.value.offset


@pytest.mark.parametrize("source", ["x = (]\n", "x = )\n", "x = 09\n"])
def test_tokenizer_public_mode_preserves_tokens_parser_mode_rejects(
    mule_tokenizer, source
):
    expected = list(
        reference_tokenize.TokenizerIter(
            io.StringIO(source).readline, extra_tokens=True
        )
    )
    assert (
        list(
            mule_tokenizer.TokenizerIter(
                io.StringIO(source).readline, extra_tokens=True
            )
        )
        == expected
    )
    with pytest.raises(SyntaxError):
        list(
            mule_tokenizer.TokenizerIter(
                io.StringIO(source).readline, extra_tokens=False
            )
        )


@pytest.mark.integration
@pytest.mark.parametrize("extra_tokens", [False, True])
def test_native_tokenizer_matches_cpython(tmp_path, extra_tokens):
    sources = [
        "π = 2.5j # note\nif π:\n    y = (1\n         + 2)\n",
        'f"{{literal}} {value!r:>{width}}"\n',
        't"hello {name}"',
    ]
    expected = [
        list(
            reference_tokenize.TokenizerIter(
                io.StringIO(source).readline, extra_tokens=extra_tokens
            )
        )
        for source in sources
    ]
    source = (
        "import _tokenize\n"
        "class Reader:\n"
        " def __init__(self, source):\n"
        "  self.lines = iter(source.splitlines(True))\n"
        " def read(self):\n"
        "  return next(self.lines, '')\n"
        f"sources = {sources!r}\n"
        f"expected = {expected!r}\n"
        "for index in range(len(sources)):\n"
        f" actual = list(_tokenize.TokenizerIter(Reader(sources[index]).read, extra_tokens={extra_tokens!r}))\n"
        " assert actual == expected[index]\n"
        "print('ok')\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"


@pytest.mark.integration
def test_native_generate_tokens_matches_cpython(tmp_path):
    text = "x = 1 # comment\nf'{x!r}'\n"
    expected = [
        tuple(item)
        for item in reference_public_tokenize.generate_tokens(
            io.StringIO(text).readline
        )
    ]
    source = (
        "import tokenize\n"
        "class Reader:\n"
        " def __init__(self, source):\n"
        "  self.lines = iter(source.splitlines(True))\n"
        " def read(self):\n"
        "  return next(self.lines, '')\n"
        f"expected = {expected!r}\n"
        f"actual = list(tokenize.generate_tokens(Reader({text!r}).read))\n"
        "assert actual == expected\n"
        "print('ok')\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"


@pytest.mark.integration
@pytest.mark.parametrize("encoding", ["utf-8", "latin-1"])
def test_native_tokenizer_encoded_lines_match_cpython(tmp_path, encoding):
    chunks = [line.encode(encoding) for line in ["é = 1\n", "x = 'é'"]]
    expected = list(
        reference_tokenize.TokenizerIter(
            iter(chunks).__next__, encoding=encoding, extra_tokens=True
        )
    )
    source = (
        "import _tokenize\n"
        f"lines = iter({chunks!r})\n"
        "def read():\n return next(lines, b'')\n"
        f"actual = list(_tokenize.TokenizerIter(read, encoding={encoding!r}, extra_tokens=True))\n"
        f"assert actual == {expected!r}\n"
        "print('ok')\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"


@pytest.mark.integration
def test_native_tokenize_bytes_matches_cpython(tmp_path):
    data = b"x = 1 # comment\nf'{x!r}'\n"
    expected = [
        tuple(item)
        for item in reference_public_tokenize.tokenize(io.BytesIO(data).readline)
    ]
    chunks = data.splitlines(True)
    source = (
        "import tokenize\n"
        f"lines = iter({chunks!r})\n"
        "def read():\n return next(lines, b'')\n"
        "actual = list(tokenize.tokenize(read))\n"
        f"assert actual == {expected!r}\n"
        "print('ok')\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"
