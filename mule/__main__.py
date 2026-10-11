"""The `mule` command: run Python programs natively, like the `python` command.

`mule script.py args`, `mule -m module args`, `mule -c command args` and
`mule - args` follow `python`: options end at the program, the remaining
arguments become `sys.argv[1:]`, and `sys.argv[0]` is the script path, the
module's file, `-c` or `-`. The program is compiled to a cached native
executable that replaces this process, so its exit status and standard streams
are the program's own. `python -m mule.compiler` builds an executable instead.
"""

import os
import sys
from dataclasses import dataclass
from pathlib import Path

from mule.diagnostics import configure, logger, phase
from mule.compiler import ToolchainError
from mule.compiler.cache import cached_executable
from mule.compiler.inputs import read_source
from mule.compiler.modules import find_module, search_path

USAGE = "usage: mule [option] ... [-c cmd | -m mod | file | -] [arg] ..."
HELP = f"""\
{USAGE}
Options:
-c cmd : program passed in as string (terminates option list)
-h     : print this help message and exit (also -? or --help)
-m mod : run library module as a script (terminates option list)
-v     : compiler progress and phase timings on stderr
-vv    : detailed compiler, input, and toolchain diagnostics on stderr
file   : program read from script file
-      : program read from stdin
arg ...: arguments passed to program in sys.argv[1:]
"""


class UsageError(Exception):
    pass


@dataclass
class Options:
    verbosity: int = 0


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


def run(kind, value, arguments):
    """Compile the selected program, then replace this process with it."""
    roots = search_path(Path.cwd())
    package = None
    if kind == "command":
        source, filename, argv0 = value, "<string>", "-c"
    elif kind == "module":
        path, package = find_module(value, roots)
        source, filename, argv0 = read_source(path), str(path), str(path.resolve())
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
    with phase("prepare executable for %s", filename):
        executable = cached_executable(source, filename, roots, package)
    logger.info("starting native program %s", executable)
    logger.debug("native argv: %r", [argv0, *arguments])
    os.execv(executable, [argv0, *arguments])


def main() -> int:
    options = Options()
    try:
        kind, value, arguments = parse(sys.argv[1:], options=options)
    except UsageError as error:
        print(f"{error}\n{USAGE}\nTry `mule -h' for more information.", file=sys.stderr)
        return 2
    configure(options.verbosity)
    if kind == "help":
        print(HELP, end="")
        return 0
    try:
        return run(kind, value, arguments)
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
