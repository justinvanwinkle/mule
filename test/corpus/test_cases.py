"""Protect metadata extraction and input isolation, without executing fixtures."""

from dataclasses import replace

import pytest

from mule.parser import parse
from test.corpus.cases import ModuleDocstring, discover_cases


def test_docstring_visitor_reads_only_the_leading_string():
    tree = parse('r"stdout = \'hello\\n\'"\nraise RuntimeError("do not execute")\n')
    assert tree.accept(ModuleDocstring()) == "stdout = 'hello\\n'"


@pytest.mark.parametrize("source", ["", "answer = 42\n", "f'hello'\n"])
def test_docstring_visitor_rejects_missing_or_nonliteral_docstring(source):
    with pytest.raises(ValueError, match="docstring"):
        parse(source).accept(ModuleDocstring())


def test_fixture_files_are_written_in_the_working_directory(tmp_path):
    case = replace(discover_cases()[0], files={"input/message.txt": "hello\n"})
    case.prepare(tmp_path)
    assert (tmp_path / "input/message.txt").read_text() == "hello\n"


@pytest.mark.parametrize("filename", ["../escape.txt", "/escape.txt"])
def test_fixture_paths_cannot_escape_working_directory(tmp_path, filename):
    case = replace(discover_cases()[0], files={filename: "hello"})
    with pytest.raises(ValueError, match="escapes"):
        case.prepare(tmp_path)


def test_empty_corpus_reports_collection_error(tmp_path):
    with pytest.raises(ValueError, match="no corpus programs"):
        discover_cases(tmp_path)


@pytest.mark.parametrize(
    ("message", "filename", "line", "matches"),
    [
        ("expected end of statement", "stdlib/library.py", 35, True),
        ("different compiler error", "stdlib/library.py", 35, False),
        ("expected end of statement", "corpus/program.py", 35, False),
        ("expected end of statement", "stdlib/library.py", 36, False),
    ],
)
def test_known_compile_gaps_do_not_mask_other_errors(
    tmp_path, message, filename, line, matches
):
    case = replace(
        discover_cases()[0],
        status="pending",
        phase="compile",
        reason="expected end of statement",
        compile_error_file=tmp_path / "stdlib/library.py",
        compile_error_line=35,
    )
    error = SyntaxError(message, (str(tmp_path / filename), line, 6, "lazy import x"))
    assert case.matches_compile_gap(error) == matches
