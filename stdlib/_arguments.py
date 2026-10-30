"""External-entry argument binding and general constructor forwarding.

Every compiled function with parameters binds a call through ``bind``, driven
by that function's constant signature tuple.
"""

from mule_types import final

__speed__ = 3
__mule_builtin_implementation__ = True

# Physical allocation, method lookup and borrowed-view invocation are primitives.
# The binder runs in the shared protocol namespace, which also supplies
# ``dict``, ``call_with``, ``is_instance`` and ``overriding_new``.
# ruff: noqa: F821


@final
def construct(cls, view):
    creator = overriding_new(cls)
    if creator is None:
        instance = _allocate_instance(cls)
    else:
        positional = [cls]
        index = 0
        while index < _binding_count(view):
            _storage_append(positional, _binding_pos(view, index))
            index += 1
        names, values = [], []
        index = 0
        while index < _binding_keyword_count(view):
            _storage_append(names, _binding_keyword_name(view, index))
            _storage_append(values, _binding_keyword_value(view, index))
            index += 1
        instance = call_with(
            creator,
            _storage_tuple(positional),
            _storage_tuple(names),
            _storage_tuple(values),
        )
        # Python initializes only instances of the class being called.
        if not is_instance(instance, cls):
            return instance
    initializer = _init_method(instance)
    if initializer is None:
        # With an overriding __new__, CPython skips the inherited
        # object.__init__ and its argument check (type_call).
        if creator is None and (
            _binding_count(view) != 0 or _binding_keyword_count(view) != 0
        ):
            raise TypeError("instance constructor takes no arguments")
    else:
        result = _forward_call(initializer, view)
        if result is not None:
            raise TypeError("__init__ must return None")
    return instance


@final
def bind(view, signature):
    """Bind an argument view to parameter order, CPython's binding rules.

    ``signature`` is ``(names, positional_only, positional, rest, defaults,
    keywords)``: parameter names in order, the counts of positional-only and
    of all positional parameters, the ``*args`` index or -1, each parameter's
    offset into the function's defaults or -1, and the ``**kwargs`` index or
    -1. Keywords no named parameter accepts are collected into a new dict.
    """
    names, positional_only, positional, rest, defaults, keywords = signature
    missing = _binding_missing()
    size = _storage_length(names)
    count = _binding_count(view)
    if rest < 0 and count > positional:
        raise TypeError("too many positional arguments")
    supplied = _binding_keyword_count(view)
    if supplied == 0 and rest < 0 and keywords < 0:
        values = []
        slot = 0
        while slot < size:
            if slot < count:
                _storage_append(values, _binding_pos(view, slot))
            else:
                offset = _storage_read(defaults, slot)
                if offset < 0:
                    raise TypeError(
                        "missing required argument " + _storage_read(names, slot)
                    )
                _storage_append(values, _binding_default(view, offset))
            slot += 1
        return _storage_tuple(values)
    values = []
    extra = None
    slot = 0
    while slot < size:
        if slot == rest:
            _storage_append(values, _binding_rest(view, positional))
        elif slot == keywords:
            extra = {}
            _storage_append(values, extra)
        elif slot < positional and slot < count:
            _storage_append(values, _binding_pos(view, slot))
        else:
            _storage_append(values, missing)
        slot += 1
    index = 0
    while index < supplied:
        name = _binding_keyword_name(view, index)
        slot = positional_only
        while slot < size and (
            slot == rest or slot == keywords or _storage_read(names, slot) != name
        ):
            slot += 1
        if slot == size:
            if extra is None:
                raise TypeError(f"got an unexpected keyword argument '{name}'")
            if name in extra:
                raise TypeError(f"got multiple values for argument '{name}'")
            extra[name] = _binding_keyword_value(view, index)
            index += 1
            continue
        if _storage_read(values, slot) is not missing:
            raise TypeError("multiple values for argument " + name)
        _storage_write(values, slot, _binding_keyword_value(view, index))
        index += 1
    slot = 0
    while slot < size:
        if _storage_read(values, slot) is missing:
            offset = _storage_read(defaults, slot)
            if offset < 0:
                raise TypeError(
                    "missing required argument " + _storage_read(names, slot)
                )
            _storage_write(values, slot, _binding_default(view, offset))
        slot += 1
    return _storage_tuple(values)
