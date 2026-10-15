"""POSIX signal dispositions and masks, compiled by Mule."""

__mule_builtin_implementation__ = True

from _mule_native import signal_limits as _limits
from _mule_native import signal_disposition as _disposition
from _mule_native import raise_signal as _raise
from _mule_native import signal_mask as _signal_mask
from _mule_native import pending_signals as _pending
from _mule_native import valid_signals as _valid
from _mule_native import wait_signal as _wait
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
SIGRTMIN, SIGRTMAX = _limits()
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
        previous = _disposition(number, int(handler))
        return default_int_handler if previous == 2 else previous
    if callable(handler):
        previous = _disposition(number, handler)
        return default_int_handler if previous == 2 else previous
    raise TypeError(
        "signal handler must be signal.SIG_IGN, signal.SIG_DFL, or a callable object"
    )


def getsignal(signalnum, /):
    previous = _disposition(_number(signalnum), None)
    return default_int_handler if previous == 2 else previous


if _disposition(SIGINT, None) == 2:
    _disposition(SIGINT, default_int_handler)


def raise_signal(signalnum, /):
    _raise(_number(signalnum))


def _mask(values):
    return tuple(_number(value) for value in values)


def pthread_sigmask(how, mask, /):
    return set(_signal_mask(_index(how), _mask(mask)))


def sigpending():
    return set(_pending())


def valid_signals():
    return set(_valid())


def sigwait(sigset, /):
    return _wait(_mask(sigset))
