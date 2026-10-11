"""Command-line entry point for building a native executable from a source file."""

import argparse
import sys
from pathlib import Path

from mule.compiler import ToolchainError, build
from mule.diagnostics import configure


def main() -> int:
    parser = argparse.ArgumentParser(
        prog="python -m mule.compiler",
        description="Compile Python to a native executable using LLVM.",
    )
    parser.add_argument("source", type=Path)
    parser.add_argument("-o", "--output", type=Path)
    parser.add_argument("--emit-llvm", type=Path, metavar="PATH")
    parser.add_argument("--clang", default="clang")
    parser.add_argument("--rustc", default="rustc")
    parser.add_argument(
        "-O",
        dest="level",
        type=int,
        choices=(0, 3),
        default=3,
        help="3: -O3 with full LTO (default); 0: -O0 without LTO, much faster to build",
    )
    parser.add_argument("-v", "--verbose", action="count", default=0)
    args = parser.parse_args()
    configure(args.verbose)
    output = args.output or args.source.with_suffix("")
    try:
        build(
            args.source,
            output,
            llvm_output=args.emit_llvm,
            clang=args.clang,
            rustc=args.rustc,
            optimize=args.level == 3,
        )
    except SyntaxError as error:
        print(
            f"{error.filename}:{error.lineno}:{error.offset}: {error.msg}",
            file=sys.stderr,
        )
        return 1
    except (OSError, ValueError, ToolchainError) as error:
        print(f"mule: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
