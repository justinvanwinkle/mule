"""Snapshot Linux terminal ABI constants from the Python 3.14 build host."""

import sys
import termios
from pathlib import Path


def main():
    if sys.platform != "linux" or sys.maxsize != (1 << 63) - 1:
        raise RuntimeError("terminal constants require a 64-bit Linux build host")
    output = Path(__file__).resolve().parents[1] / "stdlib" / "_termios_constants.py"
    lines = [
        '"""Generated Linux terminal ABI constants; regenerate with',
        'uv run tools/generate_terminal_constants.py. No host API is used at runtime."""',
        "",
    ]
    lines.extend(
        name + " = " + repr(getattr(termios, name))
        for name in sorted(dir(termios))
        if name.isupper()
    )
    output.write_text("\n".join(lines) + "\n")


if __name__ == "__main__":
    main()
