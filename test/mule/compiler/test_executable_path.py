"""The native image pathname is independent of program argument spelling."""

import subprocess

from mule.compiler import build


def test_executable_path_is_running_image_with_argument_zero_overridden(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("""
import sys
print(sys.executable)
print(sys._base_executable == sys.executable)
print(sys.argv[0])
with open(sys.executable, 'rb') as file:
    print(file.read(4) == b'\\x7fELF')
sys.argv[0] = 'replacement'
print(sys.executable)
""")
    directory = tmp_path / "image dir"
    directory.mkdir()
    executable = directory / "image"
    build(source, executable, optimize=False, workers=1)
    source.unlink()
    alias = tmp_path / "alias"
    alias.symlink_to(executable)
    # Pass a different argv[0] from the actual executable passed to execve.
    result = subprocess.run(
        ["script spelling"],
        executable=alias,
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert result.returncode == 0, result.stderr
    assert result.stderr == ""
    assert result.stdout == f"{executable}\nTrue\nscript spelling\nTrue\n{executable}\n"
