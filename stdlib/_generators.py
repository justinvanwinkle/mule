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

from _attributes import getattr
from _descriptors import property
from _mule_native import coroutine_abandon as _coroutine_abandon
from _mule_native import coroutine_new as _coroutine_new
from _mule_native import coroutine_resume as _coroutine_resume
from _mule_native import coroutine_state as _coroutine_state
from _mule_native import coroutine_throw as _coroutine_throw
from _mule_native import coroutine_yield as _coroutine_yield
from _mule_native import is_class as _is_class
from _mule_native import is_instance as _is_instance

_CREATED = 0
_RUNNING = 2
_FINISHED = 3


def _exception(kind, value):
    """Normalize ``throw``'s arguments to an exception instance."""
    if not _is_class(kind):
        return kind
    if value is None:
        return kind()
    if _is_instance(value, kind):
        return value
    return kind(value)


class generator:
    """The iterator a generator function returns."""

    def __init__(self, function, arguments, name, qualname):
        self._handle = _coroutine_new(function, arguments)
        self.__name__ = name
        self.__qualname__ = qualname

    def __iter__(self):
        return self

    def __next__(self):
        return self.send(None)

    def _resume(self, value, exception):
        try:
            if exception is None:
                result = _coroutine_resume(self._handle, value)
            else:
                result = _coroutine_throw(self._handle, exception)
        except StopIteration as error:
            if _coroutine_state(self._handle) == _FINISHED:
                raise RuntimeError("generator raised StopIteration") from error
            raise
        if _coroutine_state(self._handle) == _FINISHED:
            if result is None:
                raise StopIteration
            raise StopIteration(result)
        return result

    def send(self, value):
        state = _coroutine_state(self._handle)
        if state == _FINISHED:
            raise StopIteration
        if state == _CREATED and value is not None:
            raise TypeError("can't send non-None value to a just-started generator")
        return self._resume(value, None)

    def throw(self, kind, value=None, traceback=None):
        exception = _exception(kind, value)
        state = _coroutine_state(self._handle)
        if state == _CREATED or state == _FINISHED:
            _coroutine_abandon(self._handle)
            raise exception
        return self._resume(None, exception)

    def close(self):
        state = _coroutine_state(self._handle)
        if state == _CREATED or state == _FINISHED:
            _coroutine_abandon(self._handle)
            return None
        try:
            self._resume(None, GeneratorExit())
        except GeneratorExit, StopIteration:
            return None
        raise RuntimeError("generator ignored GeneratorExit")

    @property
    def gi_running(self):
        return _coroutine_state(self._handle) == _RUNNING

    @property
    def gi_suspended(self):
        return _coroutine_state(self._handle) == 1

    def __repr__(self):
        return f"<generator object {self.__qualname__}>"


def yield_from(iterable):
    """Delegate to a subiterator until it finishes; return its result."""
    iterator = iter(iterable)
    try:
        value = next(iterator)
    except StopIteration as stop:
        return stop.value
    while True:
        try:
            sent = _coroutine_yield(value)
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


class coroutine(generator):
    """The awaitable an ``async def`` function returns.

    It shares the generator's coroutine stack and resumption protocol;
    ``await`` drives it through ``__await__``, and it is not iterable itself.
    """

    def __iter__(self):
        raise TypeError("'coroutine' object is not iterable")

    def __next__(self):
        raise TypeError("'coroutine' object is not an iterator")

    def __await__(self):
        return _CoroutineIterator(self)

    @property
    def cr_running(self):
        return self.gi_running

    def __repr__(self):
        return f"<coroutine object {self.__qualname__}>"


class _CoroutineIterator:
    """The iterator ``coroutine.__await__`` returns: it forwards to the coroutine."""

    def __init__(self, target):
        self.target = target

    def __iter__(self):
        return self

    def __next__(self):
        return generator.send(self.target, None)

    def send(self, value):
        return generator.send(self.target, value)

    def throw(self, kind, value=None, traceback=None):
        return generator.throw(self.target, kind, value, traceback)

    def close(self):
        return generator.close(self.target)


def await_value(awaitable):
    """``await``: delegate to the awaitable's iterator; return its result."""
    method = getattr(awaitable, "__await__", None)
    if method is None:
        raise TypeError(
            f"'{type(awaitable).__name__}' object can't be awaited"
        )
    iterator = method()
    if isinstance(iterator, coroutine):
        raise TypeError("__await__() returned a coroutine")
    return yield_from(iterator)


class _AsyncGeneratorValue:
    """A value an async generator yields, as opposed to one an ``await`` in
    its body passes up to whatever drives the awaiting task."""

    def __init__(self, value):
        self.value = value


def async_generator_yield(value):
    return _coroutine_yield(_AsyncGeneratorValue(value))


class async_generator:
    """The object an ``async def`` function containing ``yield`` returns."""

    def __init__(self, function, arguments, name, qualname):
        self._body = generator(function, arguments, name, qualname)
        self.__name__ = name
        self.__qualname__ = qualname

    def __aiter__(self):
        return self

    def __anext__(self):
        return _AsyncGeneratorStep(self._body, None, None)

    def asend(self, value):
        return _AsyncGeneratorStep(self._body, value, None)

    def athrow(self, kind, value=None, traceback=None):
        return _AsyncGeneratorStep(self._body, None, _exception(kind, value))

    def aclose(self):
        return _AsyncGeneratorStep(self._body, None, GeneratorExit())

    def __repr__(self):
        return f"<async_generator object {self.__qualname__}>"


class _AsyncGeneratorStep:
    """The awaitable for one async generator step: it runs the body until
    the body yields a wrapped value, which completes the step, passing
    every other yielded value up to the awaiting task."""

    def __init__(self, body, value, exception):
        self.body = body
        self.value = value
        self.exception = exception
        self.closing = isinstance(exception, GeneratorExit)

    def __await__(self):
        return self

    def __iter__(self):
        return self

    def __next__(self):
        return self.send(None)

    def send(self, value):
        if self.exception is not None:
            exception, self.exception = self.exception, None
            return self._step(lambda: self.body.throw(exception))
        if self.value is not None:
            value, self.value = self.value, None
        return self._step(lambda: self.body.send(value))

    def throw(self, kind, value=None, traceback=None):
        exception = _exception(kind, value)
        return self._step(lambda: self.body.throw(exception))

    def close(self):
        pass

    def _step(self, resume):
        try:
            result = resume()
        except StopIteration:
            if self.closing:
                raise StopIteration
            raise StopAsyncIteration
        except GeneratorExit:
            if self.closing:
                raise StopIteration
            raise
        if isinstance(result, _AsyncGeneratorValue):
            if self.closing:
                raise RuntimeError("async generator ignored GeneratorExit")
            raise StopIteration(result.value)
        return result
