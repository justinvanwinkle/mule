"""Read test contracts from module docstrings using Mule's own syntax tree."""

import os
import re
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path

from mule import ast
from mule.parser import parse

CORPUS = Path(__file__).resolve().parents[2] / "corpus"


class ModuleDocstring(ast.Visitor):
    def visit_Node(self, node):
        raise ValueError("expected a plain string module docstring")

    def visit_Module(self, node):
        if not node.statements:
            raise ValueError("missing module docstring")
        return node.statements[0].accept(self)

    def visit_ExpressionStatement(self, node):
        return node.expression.accept(self)

    def visit_TextLiteral(self, node):
        return node.value


@dataclass(frozen=True)
class Output:
    stdout: str
    stderr: str
    exit_code: int

    @classmethod
    def from_metadata(cls, table):
        return cls(table["stdout"], table["stderr"], table["exit_code"])

    @classmethod
    def from_process(cls, process):
        return cls(process.stdout, process.stderr, process.returncode)


@dataclass(frozen=True)
class Case:
    path: Path
    name: str
    expected: Output
    stdin: str
    args: tuple[str, ...]
    timeout: float
    files: dict[str, str]
    status: str
    phase: str
    reason: str
    failure: Output | None
    compile_error_file: Path
    compile_error_line: int | None

    @property
    def id(self):
        return self.path.stem

    def matches_compile_gap(self, error):
        return (
            self.status == "pending"
            and self.phase == "compile"
            and error.msg == self.reason
            and error.filename is not None
            and Path(error.filename).resolve() == self.compile_error_file
            and (
                self.compile_error_line is None
                or error.lineno == self.compile_error_line
            )
        )

    def prepare(self, directory):
        for filename, contents in self.files.items():
            target = directory / filename
            if not target.resolve().is_relative_to(directory.resolve()):
                raise ValueError(f"fixture path escapes working directory: {filename}")
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(contents, encoding="utf-8")

    def run(self, command, directory):
        self.prepare(directory)
        environment = os.environ.copy()
        environment.pop("PYTHONPATH", None)
        environment["LC_ALL"] = "C.UTF-8"
        return Output.from_process(
            subprocess.run(
                [*command, *self.args],
                input=self.stdin,
                cwd=directory,
                env=environment,
                capture_output=True,
                text=True,
                encoding="utf-8",
                timeout=self.timeout,
                check=False,
            )
        )

    def run_reference(self, directory):
        return self.run([sys.executable, "-I", "-S", str(self.path)], directory)


def read_metadata(path):
    module = parse(path.read_text(encoding="utf-8"), filename=str(path))
    return tomllib.loads(module.accept(ModuleDocstring()))


def load_case(path):
    path = path.resolve()
    try:
        metadata = read_metadata(path)
        if metadata["schema_version"] != 1:
            raise ValueError("unsupported corpus schema version")
        for key in ("source", "expectation_source", "adaptation", "license"):
            if not metadata[key]:
                raise ValueError(f"missing {key}")
        if not re.fullmatch(r"[0-9a-f]{64}", metadata["source_sha256"]):
            raise ValueError("invalid upstream SHA256")
        license_path = path.parent / metadata["license_file"]
        if not license_path.resolve().is_relative_to(path.parent):
            raise ValueError("license path escapes corpus")
        if not license_path.is_file():
            raise ValueError("missing license file")
        test = metadata["test"]
        native = metadata["mule"]
        status = native["status"]
        phase = native.get("phase", "compile")
        reason = native["reason"]
        failure = native.get("failure")
        if status not in {"supported", "pending"}:
            raise ValueError("invalid native status")
        if phase not in {"compile", "run"}:
            raise ValueError("invalid failure phase")
        if status == "pending" and not reason:
            raise ValueError("pending cases must explain the compiler gap")
        if status == "pending" and phase == "run" and failure is None:
            raise ValueError("runtime gaps require an exact observed failure")
        if test["timeout"] <= 0:
            raise ValueError("timeout must be positive")
        return Case(
            path=path,
            name=metadata["name"],
            expected=Output.from_metadata(test),
            stdin=test["stdin"],
            args=tuple(test.get("args", ())),
            timeout=test["timeout"],
            files=metadata.get("files", {}),
            status=status,
            phase=phase,
            reason=reason,
            failure=Output.from_metadata(failure) if failure is not None else None,
            compile_error_file=(
                path.parent.parent / native["compile_error_file"]
            ).resolve()
            if "compile_error_file" in native
            else path,
            compile_error_line=native.get("compile_error_line"),
        )
    except (KeyError, ValueError) as error:
        raise ValueError(f"{path}: {error}") from error


def discover_cases(directory=CORPUS):
    paths = sorted(directory.glob("*.py"))
    if not paths:
        raise ValueError(f"no corpus programs in {directory}")
    return [load_case(path) for path in paths]
