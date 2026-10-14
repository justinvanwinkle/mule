"""Command-line entry point for building a native executable from a source file."""

import argparse
import sys
from pathlib import Path

from mule.cli_help import DefaultsHelpFormatter
from mule.compiler import ToolchainError, build
from mule.diagnostics import configure


def main() -> int:
    parser = argparse.ArgumentParser(
        prog="python -m mule.compiler",
        description="Compile Python to a native executable using LLVM.",
        formatter_class=DefaultsHelpFormatter,
    )
    parser.add_argument("source", type=Path)
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="output executable; defaults to source without its suffix",
    )
    parser.add_argument(
        "--emit-llvm", type=Path, metavar="PATH", help="save generated LLVM IR"
    )
    parser.add_argument("--clang", default="clang", help="Clang compiler executable")
    parser.add_argument("--rustc", default="rustc", help="Rust compiler executable")
    parser.add_argument(
        "-O",
        dest="level",
        type=int,
        choices=(0, 1, 2, 3),
        default=1,
        help="per-unit optimization; independent of --lto",
    )
    parser.add_argument(
        "--lto",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="enable full link-time optimization (slower to link)",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="compiler progress; repeat for detailed diagnostics",
    )
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
            optimize=args.level,
            lto=args.lto,
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
