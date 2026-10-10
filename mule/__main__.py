"""Command-line entry point for compiling a Python source file."""

import argparse
import sys
from pathlib import Path

from mule.compiler import ToolchainError, build


def main() -> int:
    parser = argparse.ArgumentParser(
        prog="mule", description="Compile Python to a native executable using LLVM."
    )
    parser.add_argument("source", type=Path)
    parser.add_argument("-o", "--output", type=Path)
    parser.add_argument("--emit-llvm", type=Path, metavar="PATH")
    parser.add_argument("--clang", default="clang")
    args = parser.parse_args()
    output = args.output or args.source.with_suffix("")
    try:
        build(args.source, output, llvm_output=args.emit_llvm, clang=args.clang)
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
