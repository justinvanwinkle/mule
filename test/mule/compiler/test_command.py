"""The `mule` command runs programs like `python`, reusing cached executables."""

import os
import subprocess
import sys

import pytest

from mule.__main__ import Options, UsageError, help_parser, parse
from mule.compiler.inputs import recording, unchanged
from mule.compiler.modules import compile_modules


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
        (["-m", "pytest", "-O0"], 1, ["-O0"]),
        (["-O0", "-m", "pytest", "-q"], 0, ["-q"]),
        (["-O0", "-O3", "s.py"], 3, []),
    ],
)
def test_parse_optimization_level_applies_only_before_the_program(
    argv, optimize, arguments
):
    options = Options()
    assert parse(argv, options=options)[2] == arguments
    assert options.optimize == optimize


@pytest.mark.parametrize(
    "argv,optimize,lto,arguments",
    (
        (["-O3", "s.py"], 3, False, []),
        (["--lto", "-O0", "s.py"], 0, True, []),
        (["-O3", "--lto", "s.py"], 3, True, []),
        (["--lto", "--no-lto", "s.py"], 1, False, []),
        (["s.py", "--lto"], 1, False, ["--lto"]),
        (["-c", "pass", "--no-lto"], 1, False, ["--no-lto"]),
    ),
)
def test_lto_is_independent_and_stops_at_program_boundary(
    argv, optimize, lto, arguments
):
    options = Options()
    assert parse(argv, options=options)[2] == arguments
    assert options.optimize == optimize
    assert options.lto is lto


def test_help_exposes_real_build_defaults():
    parser = help_parser()
    help_text = " ".join(parser.format_help().split())
    assert "(default: 1)" in help_text
    assert "--lto, --no-lto" in help_text
    assert "(default: False)" in help_text
    assert "default: None" not in help_text
    options = Options()
    assert options.optimize == 1
    assert options.lto is False


@pytest.mark.parametrize("level", (0, 1, 2, 3))
def test_all_native_optimization_levels_are_accepted(level):
    options = Options()
    parse([f"-O{level}", "script.py"], options=options)
    assert options.optimize == level


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


def test_main_step_cache_key_depends_on_content_not_filename_or_roots(tmp_path):
    from mule.compiler.artifacts import Graph
    from mule.compiler.driver import Build, Toolchain

    script = tmp_path / "sub" / "prog.py"
    script.parent.mkdir()
    script.write_text("print(42)\n")

    with Graph(1) as g1:
        b1 = Build(g1, Toolchain(optimize=2))
        g1.run(b1.builtins())
        s1 = b1.main(
            g1.file(script.resolve()),
            "print(42)\n",
            str(script.resolve()),
            (script.parent.resolve(),),
            None,
        )
    with Graph(1) as g2:
        b2 = Build(g2, Toolchain(optimize=2))
        g2.run(b2.builtins())
        s2 = b2.main(
            g2.file(script),
            "print(42)\n",
            "sub/prog.py",
            (tmp_path,),
            None,
        )
    assert s1.key == s2.key


def mule(arguments, directory, **options):
    # A private cache, so reuse is observed only for this test's builds; -O0
    # because these tests concern running programs, not optimization.
    environment = {**os.environ, "XDG_CACHE_HOME": str(directory / "cache")}
    return subprocess.run(
        [sys.executable, "-m", "mule", "-O0", *arguments],
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
    absolute = mule([str(tmp_path / "main.py")], tmp_path)
    assert absolute.stdout == f"[{str(tmp_path / 'main.py')!r}] first\n"
    assert executables(tmp_path) == built, (
        "different path spelling reuses the cached executable"
    )
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
