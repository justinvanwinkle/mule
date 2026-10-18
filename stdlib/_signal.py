"""POSIX signal dispositions and masks, compiled by Mule."""

__mule_builtin_implementation__ = True

import _mule_native
from _mule_native import signal_limits
from _mule_native import signal_disposition
from _mule_native import signal_mask
from _mule_native import pending_signals
from _mule_native import wait_signal
from math.integer import _index

SIG_DFL = 0
SIG_IGN = 1
SIG_BLOCK = 0
SIG_UNBLOCK = 1
SIG_SETMASK = 2
SIGHUP = 1
SIGINT = 2
SIGQUIT = 3
SIGILL = 4
SIGTRAP = 5
SIGABRT = SIGIOT = 6
SIGBUS = 7
SIGFPE = 8
SIGKILL = 9
SIGUSR1 = 10
SIGSEGV = 11
SIGUSR2 = 12
SIGPIPE = 13
SIGALRM = 14
SIGSTKFLT = 16
SIGTERM = 15
SIGCHLD = SIGCLD = 17
SIGCONT = 18
SIGSTOP = 19
SIGTSTP = 20
SIGTTIN = 21
SIGTTOU = 22
SIGURG = 23
SIGXCPU = 24
SIGXFSZ = 25
SIGVTALRM = 26
SIGPROF = 27
SIGWINCH = 28
SIGIO = SIGPOLL = 29
SIGPWR = 30
SIGSYS = 31
SIGRTMIN, SIGRTMAX = signal_limits()
NSIG = SIGRTMAX + 1
ITIMER_REAL = 0
ITIMER_VIRTUAL = 1
ITIMER_PROF = 2


class ItimerError(OSError):
    pass


def _number(value):
    number = _index(value)
    if number <= 0 or number >= NSIG:
        raise ValueError("signal number out of range")
    return number


def default_int_handler(*args):
    raise KeyboardInterrupt


def signal(signalnum, handler, /):
    number = _number(signalnum)
    if isinstance(handler, int) and handler in (SIG_DFL, SIG_IGN):
        previous = signal_disposition(number, int(handler))
        return default_int_handler if previous == 2 else previous
    if callable(handler):
        previous = signal_disposition(number, handler)
        return default_int_handler if previous == 2 else previous
    raise TypeError(
        "signal handler must be signal.SIG_IGN, signal.SIG_DFL, or a callable object"
    )


def getsignal(signalnum, /):
    previous = signal_disposition(_number(signalnum), None)
    return default_int_handler if previous == 2 else previous


if signal_disposition(SIGINT, None) == 2:
    signal_disposition(SIGINT, default_int_handler)


def raise_signal(signalnum, /):
    _mule_native.raise_signal(_number(signalnum))


def _mask(values):
    return tuple(_number(value) for value in values)


def pthread_sigmask(how, mask, /):
    return set(signal_mask(_index(how), _mask(mask)))


def sigpending():
    return set(pending_signals())


def valid_signals():
    return set(_mule_native.valid_signals())


def sigwait(sigset, /):
    return wait_signal(_mask(sigset))
