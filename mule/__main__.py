"""The `mule` command: run Python programs natively, like the `python` command.

`mule script.py args`, `mule -m module args`, `mule -c command args` and
`mule - args` follow `python`: options end at the program, the remaining
arguments become `sys.argv[1:]`, and `sys.argv[0]` is the script path, the
module's file, `-c` or `-`. The program is compiled to a cached native
executable that replaces this process, so its exit status and standard streams
are the program's own. `python -m mule.compiler` builds an executable instead.
"""

import argparse
import os
import sys
from dataclasses import dataclass
from pathlib import Path

from mule.compiler import ToolchainError
from mule.compiler.driver import Toolchain, build_executable
from mule.compiler.inputs import read_source
from mule.compiler.modules import find_module, search_path
from mule.diagnostics import configure, logger, phase


def help_parser():
    """The command's options, for its help and usage text. Parsing itself
    follows ``python``: options end at the program, which argparse cannot
    express, so ``parse`` interprets the arguments."""
    parser = argparse.ArgumentParser(
        prog="mule",
        usage="mule [option] ... [-c cmd | -m mod | file | -] [arg] ...",
        description="Run a Python program as a native executable.",
        add_help=False,
    )
    parser.add_argument(
        "-c", metavar="cmd", help="program passed in as string (terminates option list)"
    )
    parser.add_argument(
        "-h",
        "-?",
        "--help",
        action="store_true",
        help="print this help message and exit",
    )
    parser.add_argument(
        "-m",
        metavar="mod",
        help="run library module as a script (terminates option list)",
    )
    parser.add_argument(
        "-O",
        metavar="LEVEL",
        choices=("0", "3"),
        help="native build optimization, attached as -O0 or -O3: 3 (default) "
        "compiles with -O3 and full LTO; 0 compiles with -O0 without LTO, "
        "much faster to build",
    )
    parser.add_argument(
        "-v",
        action="count",
        help="compiler progress and phase timings on stderr; "
        "-vv adds detailed compiler, input, and toolchain diagnostics",
    )
    parser.add_argument(
        "file", nargs="?", help="program read from script file, or - for stdin"
    )
    parser.add_argument(
        "arg", nargs="*", help="arguments passed to program in sys.argv[1:]"
    )
    return parser


class UsageError(Exception):
    pass


@dataclass
class Options:
    verbosity: int = 0
    optimize: bool = True


def parse(argv, *, options=None):
    """Split `python`-style arguments into (kind, value, program arguments).

    kind is "help", "command", "module" or "script". Options end at the
    program; everything after it is passed to the program unchanged."""
    index = 0
    options = options if options is not None else Options()
    while index < len(argv):
        argument = argv[index]
        index += 1
        if argument.startswith("-v") and set(argument[1:]) == {"v"}:
            options.verbosity += len(argument) - 1
            continue
        # The level is attached: python's own -O and -OO take none.
        if argument in ("-O0", "-O3"):
            options.optimize = argument == "-O3"
            continue
        if argument in ("-h", "-?", "--help"):
            return "help", None, []
        if argument[:2] in ("-c", "-m"):
            value = argument[2:]
            if not value:
                if index == len(argv):
                    raise UsageError(f"Argument expected for the {argument} option")
                value = argv[index]
                index += 1
            kind = "command" if argument.startswith("-c") else "module"
            return kind, value, argv[index:]
        if argument == "--":
            break
        if argument != "-" and argument.startswith("-"):
            raise UsageError(f"Unknown option: {argument}")
        return "script", argument, argv[index:]
    if index < len(argv):
        return "script", argv[index], argv[index + 1 :]
    raise UsageError("interactive mode is not supported; give a script, -c, -m or -")


def run(kind, value, arguments, *, optimize=True):
    """Compile the selected program, then replace this process with it."""
    roots = search_path(Path.cwd())
    package = None
    source_path = None
    if kind == "command":
        source, filename, argv0 = value, "<string>", "-c"
    elif kind == "module":
        path, package = find_module(value, roots)
        source, filename, argv0 = read_source(path), str(path), str(path.resolve())
        source_path = path.resolve()
    elif value == "-":
        source, filename, argv0 = sys.stdin.read(), "<stdin>", "-"
    else:
        path = Path(value)
        try:
            source = read_source(path)
        except OSError as error:
            print(
                f"mule: can't open file {str(path.resolve())!r}: "
                f"[Errno {error.errno}] {error.strerror}",
                file=sys.stderr,
            )
            return 2
        filename, argv0 = value, value
        roots = search_path(path.resolve().parent)
        source_path = path.resolve()
    with phase("prepare executable for %s", filename):
        executable = build_executable(
            source,
            filename,
            roots=roots,
            package=package,
            toolchain=Toolchain(optimize=optimize),
            source_path=source_path,
        )
    logger.info("starting native program %s", executable)
    logger.debug("native argv: %r", [argv0, *arguments])
    os.execv(executable, [argv0, *arguments])


def main() -> int:
    options = Options()
    try:
        kind, value, arguments = parse(sys.argv[1:], options=options)
    except UsageError as error:
        usage = help_parser().format_usage().rstrip()
        print(f"{error}\n{usage}\nTry `mule -h' for more information.", file=sys.stderr)
        return 2
    configure(options.verbosity)
    if kind == "help":
        help_parser().print_help()
        return 0
    try:
        return run(kind, value, arguments, optimize=options.optimize)
    except SyntaxError as error:
        print(
            f"{error.filename}:{error.lineno}:{error.offset}: {error.msg}",
            file=sys.stderr,
        )
        return 1
    except (ImportError, OSError, ValueError, ToolchainError) as error:
        print(f"mule: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
