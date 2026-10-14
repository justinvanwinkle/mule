"""Compiled thread APIs report the real native main thread's identity."""

from test.mule.compiler.test_exception_groups import compare


def test_thread_and_native_identities_are_stable_and_distinct(tmp_path):
    compare(
        tmp_path,
        """
import _thread
import posix
ident = _thread.get_ident()
native = _thread.get_native_id()
print(ident > 0, ident != 1, ident == _thread.get_ident())
print(native > 0, native == posix.getpid(), native == _thread.get_native_id())
print(ident != native, ident == _thread._get_main_thread_ident())
handle = _thread._make_thread_handle(ident)
print(handle.ident == ident, handle.is_done())
lock = _thread.RLock()
print(lock.acquire(), lock._is_owned(), lock._recursion_count())
lock.release()
print(lock._is_owned(), lock._recursion_count())
""",
    )
