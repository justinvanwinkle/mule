"""POSIX dispositions, signal sets and synchronous waiting match Python 3.14."""

import signal
import subprocess

from mule.compiler.driver import build_executable
from test.mule.compiler.test_exception_groups import compare


def test_signal_constants_dispositions_and_mask_services(tmp_path):
    compare(
        tmp_path,
        """
import signal
print(signal.NSIG, signal.SIGRTMIN, signal.SIGRTMAX)
print(signal.SIGABRT is signal.SIGIOT, signal.SIGCHLD is signal.SIGCLD)
print(signal.SIGUSR1.name, int(signal.SIGUSR1), signal.SIG_BLOCK.name)
print([int(value) for value in sorted(signal.valid_signals())])
old = signal.getsignal(signal.SIGUSR1)
try:
    print(signal.signal(signal.SIGUSR1, signal.SIG_IGN) == old)
    print(signal.getsignal(signal.SIGUSR1) is signal.SIG_IGN)
finally:
    signal.signal(signal.SIGUSR1, old)
old_mask = signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGUSR1})
try:
    signal.raise_signal(signal.SIGUSR1)
    print(signal.SIGUSR1 in signal.sigpending())
    print(signal.sigwait({signal.SIGUSR1}) is signal.SIGUSR1)
    print(signal.SIGUSR1 in signal.sigpending())
finally:
    signal.pthread_sigmask(signal.SIG_SETMASK, old_mask)
for number in (0, signal.NSIG):
    try:
        signal.getsignal(number)
    except ValueError as error:
        print(str(error))
try:
    signal.default_int_handler(2, None)
except KeyboardInterrupt:
    print('ordinary KeyboardInterrupt')
""",
    )


def test_integer_representation_and_enum_string_dispatch(tmp_path):
    compare(
        tmp_path,
        """
from enum import IntEnum
class Number(IntEnum):
    value = 42
print(str(Number.value), int.__repr__(Number.value), repr(Number.value))
class Override(int):
    def __int__(self):
        return 100
value = Override(7)
print(repr(value), int.__repr__(value), int(value))
print(repr(True), repr(False), str(True), int.__repr__(True), bool.__repr__(False))
print(int.__repr__(1 << 100), int.__repr__(-(1 << 100)))
for cls in (int, bool):
    try:
        cls.__repr__('wrong')
    except TypeError as error:
        print(str(error))
""",
    )


def test_callable_signal_delivery_and_sigint_keyboard_interrupt(tmp_path):
    compare(
        tmp_path,
        """
import os
import signal
import time

print(signal.getsignal(signal.SIGINT) is signal.default_int_handler)

events = []
old_usr1 = signal.signal(signal.SIGUSR1, lambda signum, frame: events.append((signum, frame is not None)))
try:
    signal.raise_signal(signal.SIGUSR1)
    print(events)
finally:
    print(callable(signal.signal(signal.SIGUSR1, old_usr1)))

try:
    os.kill(os.getpid(), signal.SIGINT)
except KeyboardInterrupt:
    print("caught kill SIGINT")

try:
    signal.raise_signal(signal.SIGINT)
except KeyboardInterrupt:
    print("caught raise_signal SIGINT")
""",
    )


def test_uncaught_sigint_terminates_with_sigint_and_interrupts_tight_loop(tmp_path):
    exe = build_executable(
        """
import sys
sys.stdout.write("ready\\n")
sys.stdout.flush()
while True:
    pass
""",
        str(tmp_path / "loop_prog.py"),
        roots=None,
        workers=1,
    )
    proc = subprocess.Popen(
        [str(exe)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    assert proc.stdout is not None
    assert proc.stdout.readline() == "ready\n"
    proc.send_signal(signal.SIGINT)
    _, stderr = proc.communicate(timeout=5)
    assert proc.returncode == -signal.SIGINT
    assert "KeyboardInterrupt\n" in stderr
    assert "KeyboardInterrupt:" not in stderr
