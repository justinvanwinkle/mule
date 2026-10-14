"""The `mule` command runs programs like `python`, reusing cached executables."""

import os
import subprocess

import pytest

from mule.__main__ import Options, UsageError, parse
from mule.compiler.inputs import recording, unchanged
from mule.compiler.modules import compile_modules
from test.python_host import python_host


@pytest.mark.parametrize(
    ("argv", "expected"),
    [
        (["s.py", "-o", "x", "--", "-c"], ("script", "s.py", ["-o", "x", "--", "-c"])),
        (["-m", "pytest", "-q", "--", "-x"], ("module", "pytest", ["-q", "--", "-x"])),
        (["-mpkg", "one"], ("module", "pkg", ["one"])),
        (["-c", "print(1)", "-m", "x"], ("command", "print(1)", ["-m", "x"])),
        (["-cpass"], ("command", "pass", [])),
        (["-B", "s.py", "-B"], ("script", "s.py", ["-B"])),
        (["-B", "-m", "pytest", "-q"], ("module", "pytest", ["-q"])),
        (["-B", "-B", "-cpass"], ("command", "pass", [])),
        (["-B", "-", "arg"], ("script", "-", ["arg"])),
        (["-", "q"], ("script", "-", ["q"])),
        (["--", "-odd.py", "z"], ("script", "-odd.py", ["z"])),
        (["-h", "s.py"], ("help", None, [])),
    ],
)
def test_parse_stops_options_at_the_program_like_python(argv, expected):
    assert parse(argv) == expected


@pytest.mark.parametrize(
    ("argv", "optimize", "arguments"),
    [
        (["-m", "pytest", "-O0"], True, ["-O0"]),
        (["-O0", "-m", "pytest", "-q"], False, ["-q"]),
        (["-O0", "-O3", "s.py"], True, []),
    ],
)
def test_parse_optimization_level_applies_only_before_the_program(
    argv, optimize, arguments
):
    options = Options()
    assert parse(argv, options=options)[2] == arguments
    assert options.optimize is optimize


@pytest.mark.parametrize("argv", [[], ["-c"], ["-z", "s.py"], ["--"], ["-O", "s.py"]])
def test_parse_rejects_what_python_rejects_or_would_run_interactively(argv):
    with pytest.raises(UsageError):
        parse(argv)


def test_recorded_inputs_change_when_a_new_file_would_shadow_an_import(tmp_path):
    (tmp_path / "main.py").write_text("import helper\n")
    package = tmp_path / "lib"
    package.mkdir()
    (package / "helper.py").write_text("value = 1\n")
    roots = (tmp_path, package)
    with recording() as inputs:
        compile_modules("import helper\n", str(tmp_path / "main.py"), roots=roots)
    assert unchanged(inputs)
    (tmp_path / "helper.py").write_text("value = 2\n")
    assert not unchanged(inputs), "the earlier root now provides the module"


def mule(arguments, directory, **options):
    # A private cache, so reuse is observed only for this test's builds; -O0
    # because these tests concern running programs, not optimization.
    environment = {**os.environ, "XDG_CACHE_HOME": str(directory / "cache")}
    return subprocess.run(
        [python_host(), "-m", "mule", "-O0", *arguments],
        cwd=directory,
        env=environment,
        capture_output=True,
        text=True,
        timeout=600,
        check=False,
        **options,
    )


def executables(directory):
    return sorted((directory / "cache").glob("mule/steps/*/program"))


@pytest.mark.integration
def test_script_runs_with_its_arguments_and_rebuilds_after_an_import_changes(tmp_path):
    (tmp_path / "main.py").write_text(
        "import sys\nimport helper\nprint(sys.argv, helper.value)\n"
    )
    (tmp_path / "helper.py").write_text("value = 'first'\n")
    first = mule(["main.py", "-o", "x"], tmp_path)
    assert (first.returncode, first.stderr) == (0, "")
    assert first.stdout == "['main.py', '-o', 'x'] first\n"
    built = executables(tmp_path)
    assert built, "the build is cached"
    again = mule(["main.py"], tmp_path)
    assert again.stdout == "['main.py'] first\n"
    assert executables(tmp_path) == built, "unchanged inputs reuse the executable"
    (tmp_path / "helper.py").write_text("value = 'second'\n")
    edited = mule(["main.py"], tmp_path)
    assert edited.stdout == "['main.py'] second\n"


@pytest.mark.integration
def test_module_runs_as_main_with_package_relative_imports(tmp_path):
    package = tmp_path / "tool"
    package.mkdir()
    (package / "__init__.py").write_text("")
    (package / "names.py").write_text("name = 'tool'\n")
    (package / "__main__.py").write_text(
        "import sys\nfrom .names import name\n"
        "print(name, sys.argv[0] == sys.argv[2], sys.argv[1])\n"
    )
    result = mule(["-m", "tool", "arg", str(package / "__main__.py")], tmp_path)
    assert (result.returncode, result.stderr) == (0, "")
    assert result.stdout == "tool True arg\n"
