"""Validate discovery and strict reporting independently of compiler coverage."""

import subprocess
import sys

from tools.run_stdlib_tests import STRICT_RESULT, Entry, discover_entries, run_entry


def test_discovery_preserves_package_hooks_and_missing_extension_tests(tmp_path):
    root = tmp_path / "test"
    (root / "libregrtest").mkdir(parents=True)
    (root / "libregrtest/findtests.py").write_text(
        'SPLITTESTDIRS: set[str] = {"test_split"}\n'
    )
    (root / "test_split").mkdir()
    (root / "test_split/__init__.py").write_text("")
    (root / "test_split/test_one.py").write_text("")
    (root / "test_whole").mkdir()
    (root / "test_whole/__init__.py").write_text("")
    (root / "test_whole/test_nested.py").write_text("")
    (root / "test_missing_extension.py").write_text("import missing_native_extension\n")
    assert [entry.name for entry in discover_entries(tmp_path)] == [
        "test.test_split.test_one",
        "test_missing_extension",
        "test_whole",
    ]


def test_failed_compilation_is_not_an_executed_or_successful_suite(tmp_path):
    source = tmp_path / "test_invalid.py"
    source.write_text("def invalid(:\n")
    outcome = run_entry(Entry("test_invalid", source), tmp_path / "artifacts")
    assert outcome.status == "failed"
    assert outcome.phase == "test-module-compilation"
    assert outcome.tests_run == 0
    assert outcome.exit_code != 0
    assert outcome.stderr


def test_strict_result_rejects_skips_and_expected_failures(tmp_path):
    program = (
        "import unittest\nimport sys\nimport io\n"
        + STRICT_RESULT
        + "def unavailable():\n    raise unittest.SkipTest('missing extension')\n"
        "case = unittest.FunctionTestCase(unavailable)\n"
        "runner = unittest.TextTestRunner(stream=io.StringIO(), resultclass=StrictResult)\n"
        "result = runner.run(unittest.TestSuite([case]))\n"
        "print(result.testsRun, len(result.failures), result.wasSuccessful())\n"
        "result = StrictResult(unittest.runner._WritelnDecorator(io.StringIO()), True, 1)\n"
        "result.startTest(case)\n"
        "try:\n    raise AssertionError('expected upstream failure')\n"
        "except AssertionError:\n    result.addExpectedFailure(case, sys.exc_info())\n"
        "result.stopTest(case)\n"
        "print(len(result.failures), len(result.expectedFailures), result.wasSuccessful())\n"
    )
    result = subprocess.run(
        [sys.executable, "-I", "-S", "-B", "-c", program],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 1 False\n1 0 False\n"
