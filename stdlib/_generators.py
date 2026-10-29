"""The generator type and ``yield from``, compiled by Mule.

A generator owns a runtime coroutine: its body runs on a separate native
stack, and ``yield`` switches back to the code that resumed it. This module
implements CPython's generator protocol (``Objects/genobject.c``) over that
primitive: ``send``, ``throw`` and ``close``, ``StopIteration`` carrying the
return value, and PEP 479's conversion of a ``StopIteration`` escaping the
body into ``RuntimeError``. ``yield_from`` is PEP 380's delegation algorithm;
it can run outside the generator's own frame because yielding suspends the
coroutine rather than a single function.
"""

__speed__ = 3
__mule_builtin_implementation__ = True

from _attributes import getattr
from _descriptors import property
from _mule_native import coroutine_abandon
from _mule_native import coroutine_delegate
from _mule_native import coroutine_has_yieldfrom
from _mule_native import coroutine_locals
from _mule_native import coroutine_new
from _mule_native import coroutine_next
from _mule_native import coroutine_resume
from _mule_native import coroutine_set_yieldfrom
from _mule_native import coroutine_state
from _mule_native import coroutine_throw
from _mule_native import coroutine_yield
from _mule_native import coroutine_yieldfrom
from _mule_native import is_class
from _mule_native import is_instance
from _mule_native import type_of
from _object import frame
from _object import framelocalsproxy

_CREATED = 0
_RUNNING = 2
_FINISHED = 3
_MISSING = _binding_missing()


class _GeneratorFrame(frame):
    """A generator, coroutine, or async generator's frame."""

    def __init__(self, gen):
        self._gen = gen

    @property
    def f_code(self):
        return self._gen._function.__code__

    @property
    def f_lineno(self):
        return self._gen._function.__code__.co_firstlineno

    @property
    def f_globals(self):
        return self._gen._function.__globals__

    @property
    def f_back(self):
        return None

    @property
    def f_locals(self):
        return framelocalsproxy(self._gen._locals())

    def clear(self):
        if self._gen.gi_running:
            raise RuntimeError("cannot clear an executing frame")


def _exception(kind, value):
    """Normalize ``throw``'s arguments to an exception instance."""
    if not is_class(kind):
        return kind
    if value is None:
        return kind()
    if is_instance(value, kind):
        return value
    return kind(value)


class generator:
    """The iterator a generator function returns."""

    def __init__(self, function, arguments, name, qualname):
        self._handle = coroutine_new(function, arguments)
        self._function = function
        self._arguments = arguments
        self.__name__ = name
        self.__qualname__ = qualname

    def __iter__(self):
        return self

    __next__ = coroutine_next

    def _resume(self, value, exception):
        try:
            if exception is None:
                result = coroutine_resume(self._handle, value)
            else:
                result = coroutine_throw(self._handle, exception)
        except StopIteration as error:
            if coroutine_state(self._handle) == _FINISHED:
                raise RuntimeError("generator raised StopIteration") from error
            raise
        if coroutine_state(self._handle) == _FINISHED:
            if result is None:
                raise StopIteration
            raise StopIteration(result)
        return result

    def send(self, value):
        state = coroutine_state(self._handle)
        if state == _FINISHED:
            raise StopIteration
        if state == _CREATED and value is not None:
            raise TypeError("can't send non-None value to a just-started generator")
        return self._resume(value, None)

    def throw(self, kind, value=None, traceback=None):
        exception = _exception(kind, value)
        state = coroutine_state(self._handle)
        if state == _CREATED or state == _FINISHED:
            coroutine_abandon(self._handle)
            raise exception
        return self._resume(None, exception)

    def close(self):
        state = coroutine_state(self._handle)
        if state == _CREATED or state == _FINISHED:
            coroutine_abandon(self._handle)
            return None
        try:
            self._resume(None, GeneratorExit())
        except GeneratorExit:
            return None
        except StopIteration as stop:
            return stop.value
        raise RuntimeError("generator ignored GeneratorExit")

    def _locals(self):
        state = coroutine_state(self._handle)
        if state == _FINISHED:
            return {}
        names = self._function.__code__.co_varnames
        if state == _CREATED:
            values = self._arguments
        else:
            values = coroutine_locals(self._handle)
            if values is None:
                return {}
        result = {}
        for name, value in zip(names, values):
            if value is not _MISSING and not name.startswith("$"):
                result[name] = value
        return result

    @property
    def gi_code(self):
        return self._function.__code__

    @property
    def gi_frame(self):
        if coroutine_state(self._handle) == _FINISHED:
            return None
        return _GeneratorFrame(self)

    @property
    def gi_yieldfrom(self):
        return coroutine_yieldfrom(self._handle)

    @property
    def gi_running(self):
        return coroutine_state(self._handle) == _RUNNING

    @property
    def gi_suspended(self):
        return coroutine_state(self._handle) == 1

    def __repr__(self):
        return f"<generator object {self.__qualname__}>"


def yield_from(iterable):
    """Delegate to a subiterator until it finishes; return its result."""
    if type_of(iterable) is generator:
        iterator = iterable
    else:
        iterator = iter(iterable)
    if type_of(iterator) is generator:
        handle = iterator._handle
        sent = None
        has_value = False
        value = None
        while True:
            try:
                if has_value:
                    has_value = False
                    sent = coroutine_yield(value)
                    value = None
                return coroutine_delegate(handle, sent, iterator, False)
            except GeneratorExit:
                if not coroutine_has_yieldfrom():
                    raise
                coroutine_set_yieldfrom(None)
                iterator.close()
                raise
            except BaseException as error:
                if not coroutine_has_yieldfrom():
                    raise
                coroutine_set_yieldfrom(None)
                try:
                    value = iterator.throw(error)
                except StopIteration as stop:
                    return stop.value
                coroutine_set_yieldfrom(iterator)
                has_value = True
    try:
        value = next(iterator)
    except StopIteration as stop:
        return stop.value
    coroutine_set_yieldfrom(iterator)
    try:
        while True:
            try:
                sent = coroutine_yield(value)
            except GeneratorExit:
                close = getattr(iterator, "close", None)
                if close is not None:
                    close()
                raise
            except BaseException as error:
                throw = getattr(iterator, "throw", None)
                if throw is None:
                    raise
                try:
                    value = throw(error)
                except StopIteration as stop:
                    return stop.value
                continue
            try:
                if sent is None:
                    value = next(iterator)
                else:
                    value = iterator.send(sent)
            except StopIteration as stop:
                return stop.value
    finally:
        coroutine_set_yieldfrom(None)


class coroutine(generator):
    """The awaitable an ``async def`` function returns.

    It shares the generator's coroutine stack and resumption protocol;
    ``await`` drives it through ``__await__``, and it is not iterable itself.
    """

    def __iter__(self):
        raise TypeError("'coroutine' object is not iterable")

    def __next__(self):
        raise TypeError("'coroutine' object is not an iterator")

    def send(self, value):
        state = coroutine_state(self._handle)
        if state == _FINISHED:
            raise RuntimeError("cannot reuse already awaited coroutine")
        if state == _CREATED and value is not None:
            raise TypeError("can't send non-None value to a just-started coroutine")
        return self._resume(value, None)

    def __await__(self):
        return _CoroutineIterator(self)

    @property
    def cr_running(self):
        return self.gi_running

    @property
    def cr_suspended(self):
        return self.gi_suspended

    @property
    def cr_frame(self):
        return self.gi_frame

    @property
    def cr_code(self):
        return self.gi_code

    @property
    def cr_await(self):
        awaited = self.gi_yieldfrom
        if type_of(awaited) is _CoroutineIterator:
            return awaited.target
        return awaited

    def __repr__(self):
        return f"<coroutine object {self.__qualname__}>"


class _CoroutineIterator:
    """The iterator ``coroutine.__await__`` returns: it forwards to the coroutine."""

    def __init__(self, target):
        self.target = target

    def __iter__(self):
        return self

    def __next__(self):
        return coroutine.send(self.target, None)

    def send(self, value):
        return coroutine.send(self.target, value)

    def throw(self, kind, value=None, traceback=None):
        return generator.throw(self.target, kind, value, traceback)

    def close(self):
        return generator.close(self.target)


def _awaitable_iterator(awaitable):
    if is_instance(awaitable, generator) and not is_instance(awaitable, coroutine):
        if awaitable.gi_code.co_flags & 0x100:
            return awaitable
    method = getattr(awaitable, "__await__", None)
    if method is None:
        raise TypeError(
            f"'{type(awaitable).__name__}' object can't be awaited"
        )
    iterator = method()
    if is_instance(iterator, coroutine):
        raise TypeError("__await__() returned a coroutine")
    if getattr(type(iterator), "__next__", None) is None:
        raise TypeError(
            f"__await__() returned non-iterator of type '{type(iterator).__name__}'"
        )
    return iterator


def await_value(awaitable):
    """``await``: delegate to the awaitable's iterator; return its result."""
    if type_of(awaitable) is coroutine:
        handle = awaitable._handle
        sent = None
        has_value = False
        value = None
        while True:
            try:
                if has_value:
                    has_value = False
                    sent = coroutine_yield(value)
                    value = None
                return coroutine_delegate(handle, sent, awaitable, True)
            except GeneratorExit:
                if not coroutine_has_yieldfrom():
                    raise
                coroutine_set_yieldfrom(None)
                awaitable.close()
                raise
            except BaseException as error:
                if not coroutine_has_yieldfrom():
                    raise
                coroutine_set_yieldfrom(None)
                try:
                    value = awaitable.throw(error)
                except StopIteration as stop:
                    return stop.value
                coroutine_set_yieldfrom(awaitable)
                has_value = True
    return yield_from(_awaitable_iterator(awaitable))


def aiter(iterable, /):
    method = getattr(type(iterable), "__aiter__", None)
    if method is None:
        raise TypeError(f"'{type(iterable).__name__}' object is not an async iterable")
    iterator = method(iterable)
    if getattr(type(iterator), "__anext__", None) is None:
        raise TypeError(
            f"aiter() returned not an async iterator of type '{type(iterator).__name__}'"
        )
    return iterator


class _AnextAwaitable:
    def __init__(self, awaitable, default):
        self._awaitable = awaitable
        self._default = default
        self._iterator = None

    def _iter(self):
        if self._iterator is None:
            self._iterator = _awaitable_iterator(self._awaitable)
        return self._iterator

    def __await__(self):
        return self

    def __iter__(self):
        return self

    def __next__(self):
        return self.send(None)

    def send(self, value):
        try:
            iterator = self._iter()
            if value is None:
                return next(iterator)
            return iterator.send(value)
        except StopAsyncIteration:
            raise StopIteration(self._default)

    def throw(self, kind, value=None, traceback=None):
        try:
            iterator = self._iter()
            throw = getattr(iterator, "throw", None)
            if throw is None:
                raise _exception(kind, value)
            return throw(kind, value, traceback)
        except StopAsyncIteration:
            raise StopIteration(self._default)

    def close(self):
        if self._iterator is not None:
            close = getattr(self._iterator, "close", None)
            if close is not None:
                close()


def anext(iterator, default=_MISSING, /):
    method = getattr(type(iterator), "__anext__", None)
    if method is None:
        raise TypeError(f"'{type(iterator).__name__}' object is not an async iterator")
    awaitable = method(iterator)
    if default is _MISSING:
        return awaitable
    return _AnextAwaitable(awaitable, default)


class _AsyncGeneratorValue:
    """A value an async generator yields, as opposed to one an ``await`` in
    its body passes up to whatever drives the awaiting task."""

    def __init__(self, value):
        self.value = value


def async_generator_yield(value):
    return coroutine_yield(_AsyncGeneratorValue(value))


class async_generator:
    """The object an ``async def`` function containing ``yield`` returns."""

    def __init__(self, function, arguments, name, qualname):
        self._body = generator(function, arguments, name, qualname)
        self._running_async = False
        self._started = False
        self.__name__ = name
        self.__qualname__ = qualname

    def __aiter__(self):
        return self

    def __anext__(self):
        return _AsyncGeneratorStep(self, None, None)

    def asend(self, value):
        return _AsyncGeneratorStep(self, value, None)

    def athrow(self, kind, value=None, traceback=None):
        return _AsyncGeneratorStep(self, None, _exception(kind, value))

    def aclose(self):
        return _AsyncGeneratorStep(self, None, GeneratorExit())

    @property
    def ag_running(self):
        return self._running_async or self._body.gi_running

    @property
    def ag_suspended(self):
        return self._body.gi_suspended

    @property
    def ag_frame(self):
        return self._body.gi_frame

    @property
    def ag_code(self):
        return self._body.gi_code

    @property
    def ag_await(self):
        return self._body.gi_yieldfrom

    def __repr__(self):
        return f"<async_generator object {self.__qualname__}>"


class _AsyncGeneratorStep:
    """The awaitable for one async generator step: it runs the body until
    the body yields a wrapped value, which completes the step, passing
    every other yielded value up to the awaiting task."""

    def __init__(self, ag, value, exception):
        self.ag = ag
        self.body = ag._body
        self.value = value
        self.exception = exception
        self.closing = is_instance(exception, GeneratorExit)
        self._state = 0

    def __await__(self):
        return self

    def __iter__(self):
        return self

    def __next__(self):
        return self.send(None)

    def send(self, value):
        if self._state == 2:
            raise StopIteration
        if self._state == 0:
            if self.ag._running_async:
                raise RuntimeError("anext(): asynchronous generator is already running")
            if not self.closing and not self.ag._started:
                self.ag._started = True
                import sys

                hook = sys.get_asyncgen_hooks().firstiter
                if hook is not None:
                    hook(self.ag)
            self.ag._running_async = True
            self._state = 1
            if self.exception is not None:
                exception, self.exception = self.exception, None
                return self._step(lambda: self.body.throw(exception))
            if self.value is not None:
                value, self.value = self.value, None
        return self._step(lambda: self.body.send(value))

    def throw(self, kind, value=None, traceback=None):
        if self._state == 2:
            raise StopIteration
        exception = _exception(kind, value)
        if self._state == 0:
            if self.ag._running_async:
                raise RuntimeError("anext(): asynchronous generator is already running")
            if not self.closing and not self.ag._started:
                self.ag._started = True
                import sys

                hook = sys.get_asyncgen_hooks().firstiter
                if hook is not None:
                    hook(self.ag)
            self.ag._running_async = True
            self._state = 1
        return self._step(lambda: self.body.throw(exception))

    def close(self):
        if self._state == 1:
            self.ag._running_async = False
        self._state = 2

    def _step(self, resume):
        try:
            result = resume()
        except StopIteration:
            self.ag._running_async = False
            self._state = 2
            if self.closing:
                raise StopIteration
            raise StopAsyncIteration
        except StopAsyncIteration as error:
            self.ag._running_async = False
            self._state = 2
            if coroutine_state(self.body._handle) == _FINISHED:
                raise RuntimeError("async generator raised StopAsyncIteration") from error
            raise
        except GeneratorExit:
            self.ag._running_async = False
            self._state = 2
            if self.closing:
                raise StopIteration
            raise
        except BaseException:
            self.ag._running_async = False
            self._state = 2
            raise
        if is_instance(result, _AsyncGeneratorValue):
            self.ag._running_async = False
            self._state = 2
            if self.closing:
                raise RuntimeError("async generator ignored GeneratorExit")
            raise StopIteration(result.value)
        return result
