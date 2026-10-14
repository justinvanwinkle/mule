"""Fault diagnostics exercise real native file descriptors and fatal signals."""

import resource
import signal
import subprocess

from mule.compiler import build
from test.mule.compiler.test_exception_groups import compare


def test_fault_handler_state_file_selection_and_manual_dump(tmp_path):
    path = tmp_path / "dump.txt"
    compare(
        tmp_path,
        f"""
import faulthandler
import os
print(faulthandler.is_enabled())
fd = os.open({str(path)!r}, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
print(faulthandler.enable(file=fd, c_stack=False), faulthandler.is_enabled())
def outer():
    inner()
def inner():
    faulthandler.dump_traceback(file=fd)
outer()
print(faulthandler.disable(), faulthandler.is_enabled(), faulthandler.disable())
os.close(fd)
with open({str(path)!r}) as stream:
    text = stream.read()
print('most recent call first' in text, 'in inner' in text, 'in outer' in text)
""",
    )


def test_fatal_abort_reports_native_activations_and_preserves_signal_exit(tmp_path):
    source = tmp_path / "fatal.py"
    source.write_text("""
import faulthandler
import os
import signal
faulthandler.enable(c_stack=False)
def abort_here():
    os.kill(os.getpid(), signal.SIGABRT)
abort_here()
""")
    executable = build(source, tmp_path / "program", optimize=False, workers=1)
    result = subprocess.run(
        [executable],
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
        preexec_fn=lambda: resource.setrlimit(resource.RLIMIT_CORE, (0, 0)),
    )
    assert result.returncode == -signal.SIGABRT, result.stderr
    assert "Fatal Python error: Aborted" in result.stderr
    assert "in abort_here" in result.stderr
    assert str(source) in result.stderr
