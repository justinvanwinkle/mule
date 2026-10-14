"""Python argument boundaries and explicit native CLI modes."""

import pytest

from mule.cli import Mode, ProgramKind, parse


@pytest.mark.parametrize(
    ("argv", "kind", "value", "arguments"),
    [
        ([], ProgramKind.SCRIPT, "-", []),
        (["--"], ProgramKind.SCRIPT, "-", []),
        (["-", "-B"], ProgramKind.SCRIPT, "-", ["-B"]),
        (
            ["script.py", "-O", "--compile"],
            ProgramKind.SCRIPT,
            "script.py",
            ["-O", "--compile"],
        ),
        (["compile", "run"], ProgramKind.SCRIPT, "compile", ["run"]),
        (["--", "--compile", "a"], ProgramKind.SCRIPT, "--compile", ["a"]),
        (["-c", "-B", "-v"], ProgramKind.COMMAND, "-B", ["-v"]),
        (["-Bcprint(1)", "-B"], ProgramKind.COMMAND, "print(1)", ["-B"]),
        (["-m", "-B", "--run"], ProgramKind.MODULE, "-B", ["--run"]),
        (["-Impkg", "arg"], ProgramKind.MODULE, "pkg", ["arg"]),
    ],
)
def test_python_selection_preserves_program_boundary(argv, kind, value, arguments):
    command = parse(argv)
    assert command.mode == Mode.PYTHON
    assert (command.kind, command.value, command.arguments) == (kind, value, arguments)


def test_python_clustered_options_retain_their_independent_values():
    command = parse(["-bbBOOEdqsvV", "-Wignore", "-Xdev", "-cpass"])
    options = command.options
    assert options.bytes_warning == 2
    assert options.dont_write_bytecode
    assert options.optimize == 2
    assert options.ignore_environment
    assert options.debug == 1
    assert options.quiet == 1
    assert options.no_user_site
    assert options.verbose == 1
    assert options.version == 1
    assert options.warnings == ["ignore"]
    assert options.xoptions == ["dev"]
    assert command.value == "pass"


def test_python_value_options_consume_option_looking_operands():
    options = parse(["-W", "-B", "-X", "-I", "-cpass"]).options
    assert options.warnings == ["-B"]
    assert options.xoptions == ["-I"]
    assert not options.dont_write_bytecode
    assert not options.isolated


def test_python_isolation_implies_environment_and_path_restrictions():
    options = parse(["-I", "script.py"]).options
    assert options.isolated
    assert options.ignore_environment
    assert options.safe_path
    assert options.no_user_site
    assert not options.no_site


def test_python_optimization_count_is_capped_at_two():
    assert parse(["-OOOO", "script.py"]).options.optimize == 2


def test_repeated_parser_debug_and_quiet_options_remain_boolean():
    options = parse(["-ddqq", "script.py"]).options
    assert options.debug == 1
    assert options.quiet == 1


@pytest.mark.parametrize(
    ("option", "field"),
    [
        ("-i", "inspect"),
        ("-P", "safe_path"),
        ("-S", "no_site"),
        ("-u", "unbuffered"),
        ("-x", "skip_first_line"),
    ],
)
def test_python_boolean_options_are_retained(option, field):
    assert getattr(parse([option, "script.py"]).options, field)


def test_repeated_warning_and_implementation_options_preserve_order():
    options = parse(
        ["-Werror", "-W", "ignore::ImportWarning", "-Xdev", "-X", "utf8=0"]
    ).options
    assert options.warnings == ["error", "ignore::ImportWarning"]
    assert options.xoptions == ["dev", "utf8=0"]


@pytest.mark.parametrize("style", ["always", "default", "never"])
def test_hash_bytecode_policy_accepts_python_values(style):
    assert (
        parse(["--check-hash-based-pycs", style]).options.check_hash_based_pycs == style
    )


@pytest.mark.parametrize(
    ("option", "kind"),
    [
        ("-h", "help"),
        ("-?", "help"),
        ("--help", "help"),
        ("--help-env", "env"),
        ("--help-xoptions", "xoptions"),
        ("--help-all", "all"),
    ],
)
def test_help_selection_stops_before_unrelated_following_arguments(option, kind):
    assert parse([option, "--invalid"]).options.help_kind == kind


@pytest.mark.parametrize("argv", [["-VV"], ["-V", "--version"]])
def test_repeated_version_requests_are_retained(argv):
    assert parse(argv).options.version == 2


@pytest.mark.parametrize(
    "argv",
    [
        ["-c"],
        ["-m"],
        ["-W"],
        ["-X"],
        ["-O0", "a.py"],
        ["--compile-extra"],
        ["-B", "--compile", "a.py"],
        ["--check-hash-based-pycs", "invalid"],
        ["--check-hash-based-pycs=never"],
        ["--hel"],
    ],
)
def test_invalid_python_arguments_fail_before_execution(argv):
    with pytest.raises(SystemExit) as error:
        parse(argv)
    assert error.value.code == 2


def test_compile_mode_uses_explicit_native_build_arguments(tmp_path):
    source = tmp_path / "source.py"
    output = tmp_path / "program"
    llvm = tmp_path / "program.ll"
    command = parse(
        [
            "--compile",
            "--opt-level",
            "0",
            "-vv",
            "--clang",
            "clang-custom",
            "--rustc",
            "rustc-custom",
            str(source),
            "-o",
            str(output),
            "--emit-llvm",
            str(llvm),
        ]
    )
    assert command.mode == Mode.COMPILE
    assert command.options.source == source
    assert command.options.output == output
    assert command.options.emit_llvm == llvm
    assert command.options.opt_level == 0
    assert command.options.compiler_verbose == 2
    assert command.options.clang == "clang-custom"
    assert command.options.rustc == "rustc-custom"


def test_run_mode_keeps_native_optimization_separate_from_python_optimization():
    command = parse(
        ["--run", "--opt-level=0", "-v", "-OO", "-B", "-c", "print(42)", "--compile"]
    )
    assert command.mode == Mode.RUN
    assert command.options.opt_level == 0
    assert command.options.compiler_verbose == 1
    assert command.options.verbose == 0
    assert command.options.optimize == 2
    assert command.options.dont_write_bytecode
    assert command.kind == ProgramKind.COMMAND
    assert command.arguments == ["--compile"]


@pytest.mark.parametrize("mode", ("--compile", "--run"))
@pytest.mark.parametrize("level", (0, 1, 2, 3))
@pytest.mark.parametrize(
    "flags,lto", (([], False), (["--lto"], True), (["--lto", "--no-lto"], False))
)
def test_native_lto_is_separate_from_optimization(mode, level, flags, lto):
    command = parse([mode, "--opt-level", str(level), *flags, "script.py"])
    assert command.options.opt_level == level
    assert command.options.lto is lto


def test_run_mode_leaves_lto_flags_after_program_untouched():
    command = parse(["--run", "script.py", "--lto"])
    assert command.options.lto is False
    assert command.arguments == ["--lto"]


@pytest.mark.parametrize("mode", ("--compile", "--run"))
def test_native_build_defaults_are_visible_in_help(mode):
    from mule.cli import parser

    command = parse([mode, "script.py"])
    assert command.options.opt_level == 1
    assert command.options.lto is False
    help_text = " ".join(parser(command.mode).format_help().split())
    assert "(default: 1)" in help_text
    assert "(default: False)" in help_text
    assert "default: None" not in help_text


@pytest.mark.parametrize("mode", ["--compile", "--run"])
def test_native_mode_help_is_available_without_a_program(mode):
    assert parse([mode, "--help"]).options.help_kind == "help"


@pytest.mark.parametrize(
    "argv",
    [
        ["--compile"],
        ["--compile", "--opt-level", "4", "s.py"],
        ["--run", "--output", "x", "s.py"],
    ],
)
def test_native_mode_requires_valid_mode_specific_arguments(argv):
    with pytest.raises(SystemExit) as error:
        parse(argv)
    assert error.value.code == 2
