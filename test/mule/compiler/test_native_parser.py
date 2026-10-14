"""Mule's handwritten parser constructs owned trees when compiled by Mule."""

import subprocess
from pathlib import Path

import pytest

from mule.compiler.driver import Toolchain, build_executable
from mule.compiler.modules import search_path


@pytest.mark.integration
def test_owned_parser_runs_natively_and_preserves_unicode_spans(tmp_path):
    source = """
from mule.parser import parse
module = parse('result = café + 2\\n', filename='example.py')
statement = module.statements[0]
expression = statement.value
print(type(module).__name__, type(statement).__name__, type(expression).__name__)
print(statement.span.start.line, statement.span.start.column,
      statement.span.end.line, statement.span.end.column)
print(expression.left.identifier, expression.left.span.start.column,
      expression.left.span.end.column, expression.right.value)
"""
    executable = build_executable(
        source,
        str(tmp_path / "main.py"),
        roots=(
            tmp_path,
            Path(__file__).resolve().parents[3],
            *search_path(tmp_path)[1:],
        ),
        toolchain=Toolchain(optimize=False),
        workers=1,
    )
    result = subprocess.run(
        [executable], capture_output=True, text=True, timeout=10, check=False
    )
    assert result.returncode == 0, result.stderr
    assert result.stderr == ""
    assert result.stdout == "Module Assignment Binary\n1 0 1 17\ncafé 9 13 2\n"
