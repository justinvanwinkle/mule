"""Context variables, as CPython's builtin ``_contextvars`` module.

Mule runs one thread per process for now, so the current context is a
module global; ``Context.run`` swaps it for the duration of the call.
"""

from mule_types import final

__speed__ = 3
__mule_builtin_implementation__ = True


class _Missing:
    def __repr__(self):
        return "<missing>"


_MISSING = _Missing()


class Token:
    """The result of ``ContextVar.set``: restores the previous value."""

    MISSING = _MISSING

    def __init__(self, var, old_value, context):
        self._var = var
        self._old_value = old_value
        self._context = context
        self._used = False

    @property
    def var(self):
        return self._var

    @property
    def old_value(self):
        if self._old_value is _MISSING:
            raise AttributeError("token has no old value")
        return self._old_value


class ContextVar:
    """A variable whose value differs between contexts."""

    def __init__(self, name, *, default=_MISSING):
        if not isinstance(name, str):
            raise TypeError("context variable name must be a string")
        self._name = name
        self._default = default

    @property
    def name(self):
        return self._name

    def get(self, default=_MISSING):
        values = _current._values
        if self in values:
            return values[self]
        if default is not _MISSING:
            return default
        if self._default is not _MISSING:
            return self._default
        raise LookupError(self)

    def set(self, value):
        context = _current
        old = context._values.get(self, _MISSING)
        context._values[self] = value
        return Token(self, old, context)

    def reset(self, token):
        if not isinstance(token, Token) or token._var is not self:
            raise ValueError("token was created by a different ContextVar")
        if token._context is not _current:
            raise ValueError("token was created in a different Context")
        if token._used:
            raise RuntimeError("token has already been used")
        token._used = True
        if token._old_value is _MISSING:
            if self in _current._values:
                del _current._values[self]
        else:
            _current._values[self] = token._old_value

    def __repr__(self):
        return "<ContextVar name='" + self._name + "'>"


class Context:
    """A mapping of ContextVars to values, runnable as the current context."""

    def __init__(self, values=None):
        self._values = {} if values is None else values

    def run(self, function, *args, **keywords):
        global _current
        previous = _current
        _current = Context(dict(self._values))
        try:
            return function(*args, **keywords)
        finally:
            _current = previous

    def copy(self):
        return Context(dict(self._values))

    def __getitem__(self, var):
        return self._values[var]

    def get(self, var, default=None):
        return self._values.get(var, default)

    def __contains__(self, var):
        return var in self._values

    def __iter__(self):
        return iter(self._values)

    def __len__(self):
        return len(self._values)

    def keys(self):
        return self._values.keys()

    def values(self):
        return self._values.values()

    def items(self):
        return self._values.items()

    def __eq__(self, other):
        if isinstance(other, Context):
            return self._values == other._values
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __repr__(self):
        return "<Context len=" + str(len(self._values)) + ">"


_current = Context()


@final
def copy_context():
    return _current.copy()
