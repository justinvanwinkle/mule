"""Discover conformance programs and compare native Mule runs with CPython.

CPython is the oracle: each program's expected behavior is whatever the host
interpreter prints. Known Mule gaps live in `conformance/gaps.toml`, pinned to
the exact compile diagnostic or exact runtime output observed when recorded.
"""

import os
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path

from mule.compiler import build

ROOT = Path(__file__).resolve().parents[2]
CONFORMANCE = ROOT / "conformance"
GAPS = CONFORMANCE / "gaps.toml"
TIMEOUT_SECONDS = 10.0


@dataclass(frozen=True)
class Output:
    stdout: str
    stderr: str
    exit_code: int

    def matches(self, reference):
        """Compare against CPython; stderr only matters when CPython wrote none.

        Mule has no tracebacks and CPython may print compile-time warnings, so
        stderr text is not comparable when the reference produced any.
        """
        return (
            self.stdout == reference.stdout
            and self.exit_code == reference.exit_code
            and (reference.stderr != "" or self.stderr == "")
        )


@dataclass(frozen=True)
class CompileFailure:
    message: str
    file: Path | None
    line: int | None


@dataclass(frozen=True)
class Gap:
    phase: str
    reason: str
    file: Path | None = None
    line: int | None = None
    failure: Output | None = None

    def matches_compile(self, error, program_path):
        """Match a diagnostic; a gap without a file names the program itself."""
        return (
            self.phase == "compile"
            and error.message == self.reason
            and error.file == (self.file or program_path.resolve())
            and error.line == self.line
        )

    def matches_run(self, output):
        return self.phase == "run" and output == self.failure


@dataclass(frozen=True)
class Program:
    path: Path

    @property
    def id(self):
        return self.path.relative_to(CONFORMANCE).with_suffix("").as_posix()

    def run(self, command, directory, hash_seed="0"):
        environment = os.environ.copy()
        environment.pop("PYTHONPATH", None)
        environment["LC_ALL"] = "C.UTF-8"
        environment["PYTHONHASHSEED"] = hash_seed
        process = subprocess.run(
            command,
            input="",
            cwd=directory,
            env=environment,
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="backslashreplace",
            timeout=TIMEOUT_SECONDS,
            check=False,
        )
        return Output(process.stdout, process.stderr, process.returncode)

    def run_reference(self, directory, hash_seed="0"):
        return self.run(
            [sys.executable, "-I", "-S", str(self.path)], directory, hash_seed
        )

    def compile(self, executable):
        """Build natively; return the source diagnostic when Mule rejects it."""
        try:
            build(self.path, executable)
        except SyntaxError as error:
            return CompileFailure(
                message=error.msg,
                file=Path(error.filename).resolve() if error.filename else None,
                line=error.lineno,
            )
        return None


def discover_programs(directory=CONFORMANCE):
    paths = sorted(directory.rglob("*.py"))
    if not paths:
        raise ValueError(f"no conformance programs in {directory}")
    return [Program(path) for path in paths]


def load_gaps(path=GAPS):
    if not path.exists():
        return {}
    gaps = {}
    for program_id, table in tomllib.loads(path.read_text(encoding="utf-8")).items():
        phase = table["phase"]
        if phase not in {"compile", "run"}:
            raise ValueError(f"{program_id}: invalid gap phase {phase!r}")
        if not table["reason"]:
            raise ValueError(f"{program_id}: gaps must state the observed reason")
        failure = None
        if phase == "run":
            failure = Output(table["stdout"], table["stderr"], table["exit_code"])
        gaps[program_id] = Gap(
            phase=phase,
            reason=table["reason"],
            file=(ROOT / table["file"]).resolve() if "file" in table else None,
            line=table.get("line"),
            failure=failure,
        )
    return gaps


def toml_string(text):
    """Encode a TOML basic string, escaping every control character."""
    escaped = []
    for character in text:
        code = ord(character)
        if character == '"':
            escaped.append('\\"')
        elif character == "\\":
            escaped.append("\\\\")
        elif character == "\n":
            escaped.append("\\n")
        elif code < 0x20 or code == 0x7F:
            escaped.append(f"\\u{code:04X}")
        else:
            escaped.append(character)
    return '"' + "".join(escaped) + '"'


def format_gap(program_id, gap):
    lines = [f"[{toml_string(program_id)}]", f"phase = {toml_string(gap.phase)}"]
    lines.append(f"reason = {toml_string(gap.reason)}")
    if gap.file is not None:
        lines.append(f"file = {toml_string(gap.file.relative_to(ROOT).as_posix())}")
    if gap.line is not None:
        lines.append(f"line = {gap.line}")
    if gap.failure is not None:
        lines.append(f"stdout = {toml_string(gap.failure.stdout)}")
        lines.append(f"stderr = {toml_string(gap.failure.stderr)}")
        lines.append(f"exit_code = {gap.failure.exit_code}")
    return "\n".join(lines) + "\n"
