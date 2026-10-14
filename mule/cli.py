"""Python-compatible argument boundaries with explicit native operation modes.

Argparse owns option definitions and validation. The lexical pass only splits
short-option clusters and protects the program boundary and option operands:
Python accepts ``-c -B`` as source, and never parses options after that source.
"""

import argparse
from dataclasses import dataclass
from enum import StrEnum
from pathlib import Path


class Mode(StrEnum):
    PYTHON = "python"
    COMPILE = "compile"
    RUN = "run"


class ProgramKind(StrEnum):
    SCRIPT = "script"
    COMMAND = "command"
    MODULE = "module"


@dataclass
class Command:
    mode: Mode
    options: argparse.Namespace
    kind: ProgramKind
    value: str
    arguments: list[str]


@dataclass(frozen=True)
class _Option:
    flags: tuple[str, ...]
    settings: dict
    takes_value: bool = False

    def install(self, parser):
        parser.add_argument(*self.flags, **self.settings)


_PYTHON_OPTIONS = (
    _Option(
        ("-b",),
        dict(
            dest="bytes_warning",
            action="count",
            default=0,
            help="warn about bytes/string conversions and comparisons; repeat for errors",
        ),
    ),
    _Option(
        ("-B",),
        dict(
            dest="dont_write_bytecode",
            action="store_true",
            help="do not write bytecode caches (Mule never writes them)",
        ),
    ),
    _Option(
        ("-c",),
        dict(
            dest="command",
            metavar="cmd",
            help="program passed as a string; terminates interpreter options",
        ),
        True,
    ),
    _Option(
        ("-d",),
        dict(dest="debug", action="count", default=0, help="enable parser debugging"),
    ),
    _Option(
        ("-E",),
        dict(
            dest="ignore_environment",
            action="store_true",
            help="ignore PYTHON* environment variables",
        ),
    ),
    _Option(
        ("-h", "-?", "--help"),
        dict(
            dest="help_kind",
            action="store_const",
            const="help",
            help="show this help and exit",
        ),
    ),
    _Option(
        ("--help-env",),
        dict(
            dest="help_kind",
            action="store_const",
            const="env",
            help="show Python environment-variable help",
        ),
    ),
    _Option(
        ("--help-xoptions",),
        dict(
            dest="help_kind",
            action="store_const",
            const="xoptions",
            help="show implementation-option help",
        ),
    ),
    _Option(
        ("--help-all",),
        dict(
            dest="help_kind",
            action="store_const",
            const="all",
            help="show complete help",
        ),
    ),
    _Option(
        ("-i",),
        dict(
            dest="inspect",
            action="store_true",
            help="inspect interactively after running the program",
        ),
    ),
    _Option(
        ("-I",),
        dict(
            dest="isolated",
            action="store_true",
            help="isolate from the environment; implies -E, -P and -s",
        ),
    ),
    _Option(
        ("-m",),
        dict(
            dest="module",
            metavar="mod",
            help="run a module as a script; terminates interpreter options",
        ),
        True,
    ),
    _Option(
        ("-O",),
        dict(
            dest="optimize",
            action="count",
            default=0,
            help="remove assertions; repeat to also remove docstrings",
        ),
    ),
    _Option(
        ("-P",),
        dict(
            dest="safe_path",
            action="store_true",
            help="do not prepend an unsafe path to sys.path",
        ),
    ),
    _Option(
        ("-q",),
        dict(
            dest="quiet",
            action="count",
            default=0,
            help="suppress the interactive startup banner",
        ),
    ),
    _Option(
        ("-s",),
        dict(
            dest="no_user_site",
            action="store_true",
            help="do not add the user site directory",
        ),
    ),
    _Option(
        ("-S",),
        dict(dest="no_site", action="store_true", help="do not import site at startup"),
    ),
    _Option(
        ("-u",),
        dict(
            dest="unbuffered",
            action="store_true",
            help="use unbuffered stdout and stderr",
        ),
    ),
    _Option(
        ("-v",),
        dict(
            dest="verbose",
            action="count",
            default=0,
            help="trace imports; repeat for more detail",
        ),
    ),
    _Option(
        ("-V", "--version"),
        dict(
            dest="version",
            action="count",
            default=0,
            help="print version; repeat for build details",
        ),
    ),
    _Option(
        ("-W",),
        dict(
            dest="warnings",
            metavar="arg",
            action="append",
            default=[],
            help="warning control: action:message:category:module:lineno",
        ),
        True,
    ),
    _Option(
        ("-x",),
        dict(
            dest="skip_first_line",
            action="store_true",
            help="skip the first source line",
        ),
    ),
    _Option(
        ("-X",),
        dict(
            dest="xoptions",
            metavar="opt",
            action="append",
            default=[],
            help="implementation-specific option",
        ),
        True,
    ),
    _Option(
        ("--check-hash-based-pycs",),
        dict(
            choices=("always", "default", "never"),
            default="default",
            metavar="policy",
            help="hash-based bytecode cache validation policy",
        ),
        True,
    ),
)

_NATIVE_OPTIONS = (
    _Option(
        ("--opt-level",),
        dict(
            type=int,
            choices=(0, 3),
            default=3,
            help="native optimization level; 3 uses full LTO",
        ),
        True,
    ),
    _Option(
        ("--clang",),
        dict(default="clang", metavar="PATH", help="Clang compiler executable"),
        True,
    ),
    _Option(
        ("--rustc",),
        dict(default="rustc", metavar="PATH", help="Rust compiler executable"),
        True,
    ),
    _Option(
        ("-v", "--verbose"),
        dict(
            dest="compiler_verbose",
            action="count",
            default=0,
            help="compiler progress; repeat for detailed diagnostics",
        ),
    ),
)


def _options(mode: Mode) -> tuple[_Option, ...]:
    if mode == Mode.COMPILE:
        return _NATIVE_OPTIONS
    if mode == Mode.RUN:
        return (
            tuple(option for option in _PYTHON_OPTIONS if option.flags != ("-v",))
            + _NATIVE_OPTIONS
        )
    return _PYTHON_OPTIONS


def parser(mode=Mode.PYTHON):
    """Build the public mode's argparse parser, also used to render help."""
    prog = "mule" if mode == Mode.PYTHON else f"mule --{mode}"
    result = argparse.ArgumentParser(
        prog=prog,
        add_help=False,
        allow_abbrev=False,
        usage=None
        if mode == Mode.COMPILE
        else f"{prog} [option] ... [-c cmd | -m mod | file | -] [arg] ...",
        description="Compile Python to a native executable."
        if mode == Mode.COMPILE
        else "Run Python programs as native executables.",
    )
    result.set_defaults(help_kind=None, verbose=0)
    for option in _options(mode):
        option.install(result)
    if mode == Mode.COMPILE:
        result.add_argument(
            "-h",
            "--help",
            dest="help_kind",
            action="store_const",
            const="help",
            help="show this help and exit",
        )
        result.add_argument("source", type=Path, nargs="?")
        result.add_argument("-o", "--output", type=Path)
        result.add_argument("--emit-llvm", type=Path, metavar="PATH")
    else:
        result.add_argument(
            "file", nargs="?", help="script filename, or - for stdin (the default)"
        )
        result.add_argument(
            "arg", nargs="*", help="arguments passed unchanged to the program"
        )
    return result


@dataclass
class _Invocation:
    options: list[str]
    kind: ProgramKind
    value: str
    arguments: list[str]


class _PythonArguments:
    """Split interpreter options without giving program arguments to argparse."""

    def __init__(
        self, argv: list[str], mode: Mode, argument_parser: argparse.ArgumentParser
    ):
        self.argv = argv
        self.parser = argument_parser
        self.options = {
            flag: option for option in _options(mode) for flag in option.flags
        }
        self.index = 0
        self.normalized = []

    def operand(self, flag):
        if self.index == len(self.argv):
            self.parser.error(f"argument {flag}: expected one argument")
        value = self.argv[self.index]
        self.index += 1
        return value

    def selected(self, kind=ProgramKind.SCRIPT, value="-"):
        return _Invocation(self.normalized, kind, value, self.argv[self.index :])

    def option(self, flag, option, attached=None):
        if option.takes_value:
            value = self.operand(flag) if attached is None else attached
            self.normalized.append(f"{flag}={value}")
            if flag == "-c":
                return self.selected(ProgramKind.COMMAND, value)
            if flag == "-m":
                return self.selected(ProgramKind.MODULE, value)
        else:
            self.normalized.append(flag)
            if option.settings.get("dest") == "help_kind":
                return self.selected()
        return None

    def split(self):
        while self.index < len(self.argv):
            argument = self.argv[self.index]
            self.index += 1
            if argument == "--":
                value = self.operand("--") if self.index < len(self.argv) else "-"
                return self.selected(value=value)
            if argument == "-" or not argument.startswith("-"):
                return self.selected(value=argument)
            if argument.startswith("--"):
                flag, separator, attached = argument.partition("=")
                option = self.options.get(flag)
                if option is None or (separator and not option.takes_value):
                    self.parser.error(f"unrecognized argument: {argument}")
                selected = self.option(flag, option, attached if separator else None)
                if selected is not None:
                    return selected
                continue
            position = 1
            while position < len(argument):
                flag = "-" + argument[position]
                position += 1
                option = self.options.get(flag)
                if option is None:
                    self.parser.error(f"unrecognized argument: {flag}")
                attached = (
                    argument[position:]
                    if option.takes_value and position < len(argument)
                    else None
                )
                selected = self.option(flag, option, attached)
                if selected is not None:
                    return selected
                if option.takes_value:
                    break
        return self.selected()


def parse(argv):
    """Return invocation metadata; execution and startup effects belong to callers."""
    mode = Mode.PYTHON
    if argv and argv[0] in ("--compile", "--run"):
        mode = Mode(argv[0][2:])
        argv = argv[1:]
    argument_parser = parser(mode)
    if mode == Mode.COMPILE:
        options = argument_parser.parse_args(argv)
        if options.source is None and options.help_kind is None:
            argument_parser.error("the following arguments are required: source")
        return Command(
            mode, options, ProgramKind.SCRIPT, str(options.source or "-"), []
        )
    invocation = _PythonArguments(argv, mode, argument_parser).split()
    options = argument_parser.parse_args(invocation.options)
    options.optimize = min(options.optimize, 2)
    if options.isolated:
        options.ignore_environment = True
        options.safe_path = True
        options.no_user_site = True
    return Command(
        mode, options, invocation.kind, invocation.value, invocation.arguments
    )
