"""Library warning visibility must not change contradiction handling."""

import logging
import sysconfig
from pathlib import Path

import pytest

from mule.compiler import emit_llvm
from mule.compiler.main import compile_source
from mule.diagnostics import annotation_warning_level, configure

STDLIB = Path(__file__).resolve().parents[3] / "stdlib"


def test_command_default_hides_stdlib_warning_but_verbose_restores_it(
    monkeypatch, capsys
):
    logger = logging.getLogger("mule")
    monkeypatch.setattr(logger, "handlers", list(logger.handlers))
    monkeypatch.setattr(logger, "level", logger.level)
    monkeypatch.setattr(logger, "propagate", logger.propagate)
    source = "def declaration() -> int:\n return 'wrong'\n"
    configure(0)
    compile_source(source, str(STDLIB / "abc.py"))
    assert "the annotation is ignored" not in capsys.readouterr().err
    compile_source(source, "application.py")
    assert "the annotation is ignored" in capsys.readouterr().err
    configure(1)
    compile_source(source, str(STDLIB / "abc.py"))
    assert "the annotation is ignored" in capsys.readouterr().err


@pytest.mark.parametrize(
    "filename",
    (
        STDLIB / "abc.py",
        STDLIB / "bootstrap" / "unittest.py",
        Path(sysconfig.get_path("stdlib")) / "abc.py",
    ),
)
def test_stdlib_annotation_warnings_are_verbose_only(caplog, filename):
    logger = logging.getLogger("mule.compiler.main")
    logger.addHandler(caplog.handler)
    try:
        with caplog.at_level(logging.INFO, logger=logger.name):
            compilation = compile_source(
                "def declaration() -> int:\n return 'wrong'\n", str(filename)
            )
    finally:
        logger.removeHandler(caplog.handler)
    records = [
        record
        for record in caplog.records
        if "the annotation is ignored" in record.message
    ]
    assert records
    assert all(record.levelno == logging.INFO for record in records)
    declaration = next(
        function
        for function in compilation.lambdas
        if function.source_name == "declaration"
    )
    assert declaration.signature.return_annotation is None


@pytest.mark.parametrize(
    "filename",
    (
        "application.py",
        Path(sysconfig.get_path("purelib")) / "library.py",
        STDLIB.parent / "stdlib-lookalike" / "library.py",
    ),
)
def test_application_and_third_party_warnings_stay_visible(filename):
    assert annotation_warning_level(str(filename)) == logging.WARNING


def test_actual_storage_errors_are_not_suppressed_by_library_origin():
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3)\n"
        "class Record:\n"
        " value: int64\n"
        " def __init__(self):\n"
        "  self.value = 'wrong'\n"
    )
    with pytest.raises(SyntaxError, match="contradicts annotation"):
        emit_llvm(source, str(STDLIB / "record.py"))
