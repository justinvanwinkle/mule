"""External-entry argument binding and general constructor forwarding.

Every compiled function with parameters binds a call through ``bind``, driven
by that function's constant signature tuple.
"""

# Physical allocation, method lookup and borrowed-view invocation are primitives.
# ruff: noqa: F821


def construct(cls, view):
    instance = _allocate_instance(cls)
    initializer = _init_method(instance)
    if initializer is None:
        if _binding_count(view) != 0 or _binding_keyword_count(view) != 0:
            raise TypeError("instance constructor takes no arguments")
    else:
        result = _forward_call(initializer, view)
        if result is not None:
            raise TypeError("__init__ must return None")
    return instance


def bind(view, signature):
    """Bind an argument view to parameter order, CPython's binding rules.

    ``signature`` is ``(names, positional_only, positional, rest, defaults)``:
    parameter names in order, the counts of positional-only and of all
    positional parameters, the ``*args`` index or -1, and each parameter's
    offset into the function's defaults or -1.
    """
    names, positional_only, positional, rest, defaults = signature
    missing = _binding_missing()
    size = _storage_length(names)
    count = _binding_count(view)
    if rest < 0 and count > positional:
        raise TypeError("too many positional arguments")
    values = []
    slot = 0
    while slot < size:
        if slot == rest:
            _storage_append(values, _binding_rest(view, positional))
        elif slot < positional and slot < count:
            _storage_append(values, _binding_pos(view, slot))
        else:
            _storage_append(values, missing)
        slot += 1
    keywords = _binding_keyword_count(view)
    index = 0
    while index < keywords:
        name = _binding_keyword_name(view, index)
        slot = positional_only
        while slot < size and (slot == rest or _storage_read(names, slot) != name):
            slot += 1
        if slot == size:
            raise TypeError("unexpected keyword argument")
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
