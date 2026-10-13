"""Low-level import machinery, as CPython's builtin ``_imp`` module.

Mule loads compiled modules itself; this provides the lock, builtin and
frozen queries, and extension suffixes in compiled Python.
"""

import sys as _sys

check_hash_based_pycs = 'check_source'
pyc_magic_number_token = 0

_locked = False


def acquire_lock():
    global _locked
    _locked = True


def release_lock():
    global _locked
    _locked = False


def lock_held():
    return _locked


def is_builtin(name):
    return name in _sys.builtin_module_names


def is_frozen(name):
    return False


def is_frozen_package(name):
    return False


def find_frozen(name):
    return None


def get_frozen_object(name):
    raise ImportError(f'No such frozen object named {name}')


def create_builtin(spec):
    raise ImportError(f'No builtin module named {spec.name}')


def exec_builtin(module):
    return 0


def create_dynamic(spec):
    raise ImportError(f'No extension module named {spec.name}')


def exec_dynamic(module):
    return 0


def extension_suffixes():
    return ['.cpython-314-x86_64-linux-gnu.so', '.abi3.so', '.so']


def source_hash(key, source_bytes):
    result = 0
    data = bytes(source_bytes)
    for index in range(len(data)):
        result = (result * 131 + data[index]) & 0xFFFFFFFFFFFFFFFF
    return result.to_bytes(8, 'little')


def _fix_co_filename(code, filename):
    return None


def _override_multi_interp_extensions_check(override):
    return 0
