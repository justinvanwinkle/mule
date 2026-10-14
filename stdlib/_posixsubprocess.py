"""Subprocess argument preparation for the native POSIX spawn boundary."""

from _mule_native import fork_exec as _fork
from math.integer import _index
import os


def _integer(value):
    value = _index(value)
    if value < -(1 << 31) or value >= (1 << 31):
        raise OverflowError("Python int too large to convert to C int")
    return value


def _id(value):
    if value is None:
        return None
    value = _index(value)
    if value < 0 or value >= (1 << 32):
        raise OverflowError("identity does not fit a native unsigned int")
    return value


def fork_exec(
    args,
    executable_list,
    close_fds,
    fds_to_keep,
    cwd,
    env,
    p2cread,
    p2cwrite,
    c2pread,
    c2pwrite,
    errread,
    errwrite,
    errpipe_read,
    errpipe_write,
    restore_signals,
    start_new_session,
    process_group,
    gid,
    gids,
    uid,
    umask,
    preexec_fn,
    /,
):
    if preexec_fn is not None:
        raise NotImplementedError(
            "compiled subprocess pre-exec callbacks are not implemented"
        )
    keep = tuple(_integer(fd) for fd in fds_to_keep)
    if any(fd < 0 for fd in keep) or tuple(sorted(set(keep))) != keep:
        raise ValueError("bad value(s) in fds_to_keep")
    fds = tuple(
        _integer(fd)
        for fd in (
            p2cread,
            p2cwrite,
            c2pread,
            c2pwrite,
            errread,
            errwrite,
            errpipe_read,
            errpipe_write,
        )
    )
    if fds[-1] < 0 or fds[-1] not in keep:
        raise ValueError("the exec error descriptor must be preserved")
    arguments = tuple(os.fsencode(value) for value in args)
    candidates = tuple(os.fsencode(value) for value in executable_list)
    if not arguments or not candidates:
        raise ValueError("argument and executable vectors must not be empty")
    environment = None if env is None else tuple(bytes(value) for value in env)
    return _fork(
        arguments,
        candidates,
        bool(close_fds),
        keep,
        None if cwd is None else os.fsencode(cwd),
        environment,
        fds,
        bool(restore_signals),
        bool(start_new_session),
        _integer(process_group),
        _id(gid),
        None if gids is None else tuple(_id(value) for value in gids),
        _id(uid),
        _integer(umask),
    )
