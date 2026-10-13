"""Compiled SystemExit policy handles native integer storage and subclasses."""

import subprocess

from mule.compiler import build


def test_exit_status_and_messages_match_python_native_long_boundary(tmp_path):
    source = tmp_path / "main.py"
    source.write_text(
        "import sys\n"
        "from enum import IntEnum\n"
        "class Status(IntEnum):\n success = 0\n failure = 7\n"
        "class Integer(int):\n pass\n"
        "codes = [None, True, False, 7, -1, 256, 2 ** 62, 2 ** 100, -2 ** 100,\n"
        " Status.success, Status.failure, Integer(3), 'message', (1, 2)]\n"
        "raise SystemExit(codes[int(sys.argv[1])])\n"
    )
    executable = build(source, tmp_path / "program", optimize=False, workers=1)
    expected = [
        (0, ""),
        (1, ""),
        (0, ""),
        (7, ""),
        (255, ""),
        (0, ""),
        (0, ""),
        (255, ""),
        (255, ""),
        (0, ""),
        (7, ""),
        (3, ""),
        (1, "message\n"),
        (1, "(1, 2)\n"),
    ]
    for index, (status, message) in enumerate(expected):
        result = subprocess.run(
            [executable, str(index)],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        assert result.returncode == status, result.stderr
        assert result.stdout == ""
        assert result.stderr == message
