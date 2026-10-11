"""Attempt every released standard-library test suite as native Mule code."""

import argparse
import json
import subprocess
import sys
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import asdict, dataclass
from pathlib import Path

from mule import ast
from mule.parser import parse
from tools import stdlib_bootstrap

ROOT = Path(__file__).resolve().parents[1]
STDLIB = ROOT / "stdlib"

# This code runs inside the compiled test executable, never as a host fallback.
STRICT_RESULT = """
class StrictResult(unittest.TextTestResult):
    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        self.failures.append((test, "Unavailable test: " + reason))

    def addExpectedFailure(self, test, error):
        self.addFailure(test, error)
"""


class StringSet(ast.Visitor):
    def visit_Node(self, node):
        raise ValueError("upstream split-test declaration is not a string set")

    def visit_Set(self, node):
        return {element.accept(self) for element in node.elements}

    def visit_TextLiteral(self, node):
        return node.value


class SplitDirectories(ast.Visitor):
    def visit_Module(self, node):
        for statement in node.statements:
            result = statement.accept(self)
            if result is not None:
                return result
        raise ValueError("upstream SPLITTESTDIRS declaration is missing")

    def visit_AnnotatedAssignment(self, node):
        if node.target.binding_name() == "SPLITTESTDIRS":
            return node.value.accept(StringSet())


@dataclass(frozen=True)
class Entry:
    name: str
    source: Path


def discover_entries(stdlib=STDLIB):
    declaration = stdlib / "test/libregrtest/findtests.py"
    split = parse(declaration.read_text(), filename=str(declaration)).accept(
        SplitDirectories()
    )

    def find(directory, base=""):
        entries = []
        for path in sorted(directory.iterdir()):
            name = path.stem if path.suffix == ".py" else path.name
            if not name.startswith("test_"):
                continue
            relative = f"{base}.{name}" if base else name
            if relative in split:
                next_base = relative if base else f"test.{relative}"
                entries.extend(find(path, next_base))
            elif path.is_dir():
                entries.append(Entry(relative, path / "__init__.py"))
            elif path.suffix == ".py":
                entries.append(Entry(relative, path))
        return entries

    entries = find(stdlib / "test")
    if not entries:
        raise ValueError("no upstream standard-library tests found")
    for entry in entries:
        if not entry.source.is_file():
            raise ValueError(f"missing upstream test source: {entry.source}")
    return sorted(entries, key=lambda entry: entry.name)


def driver_source(entry, result_file, stdlib=STDLIB):
    name = entry.name if entry.name.startswith("test.") else "test." + entry.name
    return (
        "import sys\n"
        f"sys.path.insert(0, {str(stdlib)!r})\n"
        "import importlib\nimport json\nimport unittest\n"
        "from test import support\n"
        # None enables all 15 resources, including four omitted by upstream -uall.
        "support.use_resources = None\n"
        + STRICT_RESULT
        + f"module = importlib.import_module({name!r})\n"
        "if hasattr(module, 'test_main'):\n"
        "    raise RuntimeError('legacy test_main is not supported by Python 3.15 regrtest')\n"
        "loader = unittest.TestLoader()\n"
        "suite = loader.loadTestsFromModule(module)\n"
        "runner = unittest.TextTestRunner(resultclass=StrictResult, verbosity=1)\n"
        "result = runner.run(suite)\n"
        "stats = {'tests_run': result.testsRun, 'failures': len(result.failures),\n"
        "         'errors': len(result.errors), 'unavailable': len(result.skipped),\n"
        "         'loader_errors': loader.errors}\n"
        f"with open({str(result_file)!r}, 'w', encoding='utf-8') as output:\n"
        "    json.dump(stats, output)\n"
        "if loader.errors or result.testsRun == 0 or not result.wasSuccessful():\n"
        "    sys.exit(1)\n"
    )


@dataclass(frozen=True)
class Outcome:
    name: str
    source: str
    status: str
    phase: str
    stdout: str
    stderr: str
    exit_code: int | None
    tests_run: int = 0
    stats: dict | None = None


def compile_source(source, executable, timeout):
    return subprocess.run(
        [
            sys.executable,
            "-B",
            "-m",
            "mule.compiler",
            str(source),
            "-o",
            str(executable),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        encoding="utf-8",
        timeout=timeout,
        check=False,
    )


def run_entry(entry, directory, timeout=120.0):
    """Validate the test source, then compile and execute its unittest driver."""
    directory.mkdir(parents=True, exist_ok=True)
    phase = "test-module-compilation"
    try:
        cases = stdlib_bootstrap.discover_cases(
            entry.source.read_text(encoding="utf-8"), str(entry.source)
        )
        if cases is not None:
            return run_bootstrap(entry, cases, directory, timeout)
        # This executable is not run as a script: doing that could report green
        # without loading any tests. It validates the actual upstream source.
        compiled = compile_source(entry.source, directory / "module", timeout)
        if compiled.returncode:
            return Outcome(
                entry.name,
                str(entry.source),
                "failed",
                phase,
                compiled.stdout,
                compiled.stderr,
                compiled.returncode,
            )
        phase = "test-driver-compilation"
        result_file = directory / "test-result.json"
        result_file.unlink(missing_ok=True)
        driver = directory / "driver.py"
        executable = directory / "run-tests"
        driver.write_text(driver_source(entry, result_file))
        compiled = compile_source(driver, executable, timeout)
        if compiled.returncode:
            return Outcome(
                entry.name,
                str(entry.source),
                "failed",
                phase,
                compiled.stdout,
                compiled.stderr,
                compiled.returncode,
            )
        with executable.open("rb") as binary:
            if binary.read(4) != b"\x7fELF":
                raise ValueError("compiler output is not a native ELF executable")
        phase = "test-execution"
        result = subprocess.run(
            [str(executable)],
            cwd=directory,
            capture_output=True,
            text=True,
            encoding="utf-8",
            timeout=timeout,
            check=False,
        )
        stats = json.loads(result_file.read_text()) if result_file.is_file() else None
        tests_run = stats.get("tests_run", 0) if stats is not None else 0
        passed = (
            result.returncode == 0
            and tests_run > 0
            and stats["failures"] == 0
            and stats["errors"] == 0
            and stats["unavailable"] == 0
            and not stats["loader_errors"]
        )
        return Outcome(
            entry.name,
            str(entry.source),
            "passed" if passed else "failed",
            phase,
            result.stdout,
            result.stderr,
            result.returncode,
            tests_run,
            stats,
        )
    except subprocess.TimeoutExpired:
        return Outcome(
            entry.name,
            str(entry.source),
            "failed",
            phase,
            "",
            f"Timed out after {timeout} seconds",
            None,
        )
    except (OSError, ValueError, KeyError) as error:
        # An orchestration failure is a real failed entry; continue attempting
        # the other suites and preserve the diagnostic in the report.
        return Outcome(
            entry.name,
            str(entry.source),
            "failed",
            phase,
            "",
            str(error),
            None,
        )


def run_bootstrap(entry, cases, directory, timeout):
    module_name = entry.name if entry.name.startswith("test.") else "test." + entry.name
    driver = directory / "driver.py"
    executable = directory / "run-tests"
    driver.write_text(stdlib_bootstrap.driver_source(module_name, cases))
    compiled = compile_source(driver, executable, timeout)
    if compiled.returncode:
        return Outcome(
            entry.name,
            str(entry.source),
            "failed",
            "test-driver-compilation",
            compiled.stdout,
            compiled.stderr,
            compiled.returncode,
        )
    with executable.open("rb") as binary:
        if binary.read(4) != b"\x7fELF":
            raise ValueError("compiler output is not a native ELF executable")
    results = []
    for case in cases:
        try:
            result = subprocess.run(
                [str(executable)],
                input=case.name + "\n",
                cwd=directory,
                capture_output=True,
                text=True,
                timeout=timeout,
                check=False,
            )
            lines = result.stdout.splitlines()
            executed = "MULE_TEST_START " + case.name in lines
            passed = (
                executed
                and result.returncode == 0
                and "MULE_TEST_PASS " + case.name in lines
            )
            failure = executed and result.stderr.startswith("AssertionError:")
            results.append(
                {
                    "name": case.name,
                    "executed": executed,
                    "passed": passed,
                    "failure": failure,
                    "stdout": result.stdout,
                    "stderr": result.stderr,
                    "exit_code": result.returncode,
                }
            )
        except subprocess.TimeoutExpired as error:
            stdout = (error.stdout or b"").decode("utf-8", errors="replace")
            results.append(
                {
                    "name": case.name,
                    "executed": "MULE_TEST_START " + case.name in stdout.splitlines(),
                    "passed": False,
                    "failure": False,
                    "stdout": stdout,
                    "stderr": f"Timed out after {timeout} seconds",
                    "exit_code": None,
                }
            )
    tests_run = sum(result["executed"] for result in results)
    passed = all(result["passed"] for result in results)
    stats = {
        "tests_run": tests_run,
        "failures": sum(result["failure"] for result in results),
        "errors": sum(
            not result["passed"] and not result["failure"] for result in results
        ),
        "unavailable": 0,
        "loader_errors": [],
        "cases": results,
        "runner": "compiled-bootstrap",
    }
    (directory / "test-result.json").write_text(json.dumps(stats, indent=2) + "\n")
    return Outcome(
        entry.name,
        str(entry.source),
        "passed" if passed else "failed",
        "test-execution",
        "".join(result["stdout"] for result in results),
        "".join(result["stderr"] for result in results),
        0 if passed else 1,
        tests_run,
        stats,
    )


def run_all(entries, output, *, jobs=6, timeout=120.0):
    output.mkdir(parents=True, exist_ok=True)
    outcomes = []
    with ThreadPoolExecutor(max_workers=jobs) as executor:
        pending = {
            executor.submit(run_entry, entry, output / entry.name, timeout): entry
            for entry in entries
        }
        for future in as_completed(pending):
            outcome = future.result()
            outcomes.append(outcome)
            (output / outcome.name / "outcome.json").write_text(
                json.dumps(asdict(outcome), indent=2) + "\n"
            )
            if len(outcomes) % 50 == 0 or len(outcomes) == len(entries):
                print(f"Attempted {len(outcomes)}/{len(entries)} suites", flush=True)
    outcomes.sort(key=lambda outcome: outcome.name)
    summary = {
        "entries_attempted": len(outcomes),
        "entries_passed": sum(outcome.status == "passed" for outcome in outcomes),
        "entries_failed": sum(outcome.status != "passed" for outcome in outcomes),
        "entries_executed": sum(
            outcome.phase == "test-execution" for outcome in outcomes
        ),
        "individual_tests_run": sum(outcome.tests_run for outcome in outcomes),
        "failure_phases": dict(
            Counter(outcome.phase for outcome in outcomes if outcome.status != "passed")
        ),
    }
    report = {"summary": summary, "results": [asdict(outcome) for outcome in outcomes]}
    (output / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(summary, indent=2))
    print(f"Full report: {output / 'results.json'}")
    return 1 if summary["entries_failed"] else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--list", action="store_true", help="List upstream-discovered suites."
    )
    parser.add_argument("--jobs", type=int, default=6)
    parser.add_argument(
        "--suite",
        action="append",
        help="Run a named upstream entry (repeatable); the default runs all entries.",
    )
    parser.add_argument("--timeout", type=float, default=120.0)
    parser.add_argument("--output", type=Path, default=ROOT / "build/stdlib-tests")
    arguments = parser.parse_args()
    if arguments.jobs < 1 or arguments.timeout <= 0:
        parser.error("jobs and timeout must be positive")
    entries = discover_entries()
    if arguments.suite:
        requested = set(arguments.suite)
        known = {entry.name for entry in entries}
        if requested - known:
            parser.error(
                "unknown upstream entries: " + ", ".join(sorted(requested - known))
            )
        entries = [entry for entry in entries if entry.name in requested]
    if arguments.list:
        for entry in entries:
            print(entry.name)
        return 0
    return run_all(
        entries,
        arguments.output.resolve(),
        jobs=arguments.jobs,
        timeout=arguments.timeout,
    )


if __name__ == "__main__":
    raise SystemExit(main())
