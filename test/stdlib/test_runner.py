"""Validate discovery and strict reporting independently of compiler coverage."""

import shutil
import subprocess
import sys

from tools.run_stdlib_tests import STRICT_RESULT, Entry, discover_entries, run_entry
from tools.stdlib_bootstrap import discover_cases


def _external_python_host() -> str:
    if sys.implementation.name == "mule":
        return shutil.which("python3") or "python3"
    return sys.executable


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
        [_external_python_host(), "-I", "-S", "-B", "-c", program],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 1 False\n1 0 False\n"


def test_bootstrap_discovery_retains_every_method_and_rejects_fixtures():
    source = "import unittest\nclass Cases(unittest.TestCase):\n def test_first(self):\n  self.assertEqual(1, 1)\n def test_second(self):\n  self.fail('failure')\n"
    cases = discover_cases(source, "cases.py")
    assert [case.name for case in cases] == ["Cases.test_first", "Cases.test_second"]
    assert discover_cases(source + " def setUp(self):\n  pass\n", "cases.py") is None
    assert (
        discover_cases(
            source + "def load_tests(loader, tests, pattern):\n return tests\n",
            "cases.py",
        )
        is None
    )


def test_native_bootstrap_records_assertions_and_errors_and_continues(tmp_path):
    package = tmp_path / "test"
    package.mkdir()
    (package / "__init__.py").write_text("")
    source = package / "test_native_fixture.py"
    source.write_text(
        "import unittest\nclass Cases(unittest.TestCase):\n"
        " def test_a_failure(self):\n  self.assertEqual(1, 2)\n"
        " def test_b_error(self):\n  self.assertEqual(missing, 2)\n"
        " def test_c_success(self):\n  self.assertEqual(2 ** 100, 1267650600228229401496703205376)\n"
    )
    outcome = run_entry(Entry("test_native_fixture", source), tmp_path)
    assert outcome.phase == "test-execution", outcome.stderr
    assert outcome.status == "failed"
    assert outcome.tests_run == 3
    assert outcome.stats["failures"] == 1
    assert outcome.stats["errors"] == 1
    assert [case["passed"] for case in outcome.stats["cases"]] == [False, False, True]
    assert "AssertionError: 1 != 2" in outcome.stderr
    assert "NameError" in outcome.stderr
