"""Ordinary compiled exception group classes and recursive tree selection."""


def _predicate(condition):
    error = (
        "expected an exception type, a tuple of exception types, "
        "or a callable (other than a class)"
    )
    if isinstance(condition, type):
        if not issubclass(condition, BaseException):
            raise TypeError(error)
        return lambda exception: isinstance(exception, condition)
    if isinstance(condition, tuple):
        for candidate in condition:
            if not isinstance(candidate, type) or not issubclass(
                candidate, BaseException
            ):
                raise TypeError(error)
        return lambda exception: isinstance(exception, condition)
    if not callable(condition):
        raise TypeError(error)
    return condition


class BaseExceptionGroup(BaseException):
    def __new__(cls, message, exceptions, /):
        if not isinstance(message, str):
            raise TypeError(
                "BaseExceptionGroup.__new__() argument 1 must be str, not "
                + type(message).__name__
            )
        if (
            isinstance(exceptions, dict)
            or getattr(type(exceptions), "__getitem__", None) is None
        ):
            raise TypeError("second argument (exceptions) must be a sequence")
        children = tuple(exceptions)
        if not children:
            raise ValueError(
                "second argument (exceptions) must be a non-empty sequence"
            )
        ordinary = True
        for index, exception in enumerate(children):
            if not isinstance(exception, BaseException):
                raise ValueError(
                    f"Item {index} of second argument (exceptions) is not an exception"
                )
            if not isinstance(exception, Exception):
                ordinary = False
        if issubclass(cls, Exception) and not ordinary:
            raise TypeError("Cannot nest BaseExceptions in an ExceptionGroup")
        if cls is BaseExceptionGroup and ordinary:
            cls = ExceptionGroup
        instance = object.__new__(cls)
        BaseException.__init__(instance, message, exceptions)
        instance._message = message
        instance._exceptions = children
        return instance

    @property
    def message(self):
        return self._message

    @property
    def exceptions(self):
        return self._exceptions

    def __str__(self):
        size = len(self.exceptions)
        suffix = "" if size == 1 else "s"
        return f"{self.message} ({size} sub-exception{suffix})"

    def derive(self, exceptions, /):
        return BaseExceptionGroup(self.message, exceptions)

    def _derive_copy(self, exceptions):
        derived = self.derive(exceptions)
        if not isinstance(derived, BaseExceptionGroup):
            raise TypeError("derive must return an instance of BaseExceptionGroup")
        derived.__traceback__ = self.__traceback__
        derived.__cause__ = self.__cause__
        derived.__context__ = self.__context__
        derived.__suppress_context__ = self.__suppress_context__
        notes = getattr(self, "__notes__", None)
        if notes is not None:
            derived.__notes__ = list(notes)
        return derived

    def _partition(self, predicate, include_rest):
        if predicate(self):
            return self, None
        matching, rest = [], []
        for exception in self.exceptions:
            if isinstance(exception, BaseExceptionGroup):
                match, remainder = exception._partition(predicate, include_rest)
            elif predicate(exception):
                match, remainder = exception, None
            else:
                match, remainder = None, exception
            if match is not None:
                matching.append(match)
            if remainder is not None:
                rest.append(remainder)
        match = self._derive_copy(matching) if matching else None
        remainder = self._derive_copy(rest) if include_rest and rest else None
        return match, remainder

    def subgroup(self, condition, /):
        return self._partition(_predicate(condition), False)[0]

    def split(self, condition, /):
        return self._partition(_predicate(condition), True)


class ExceptionGroup(BaseExceptionGroup, Exception):
    pass


BaseExceptionGroup.__module__ = "builtins"
ExceptionGroup.__module__ = "builtins"
