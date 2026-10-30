"""Object reduction and state policy, compiled by Mule."""

import copyreg
from _object import (
    bind_instance_attribute,
    class_lookup,
    class_lookup_default,
    instance_extra_items,
    instance_items,
    _index_integer,
    storage_dictionary,
    type_of,
    object,
)
from mule_types import final

__speed__ = 3
__mule_builtin_implementation__ = True


@final
def getstate(value):
    owner = type_of(value)
    slotnames = copyreg._slotnames(owner)
    try:
        namespace = storage_dictionary(value)
    except AttributeError:
        state = None
    else:
        if namespace is None:
            extras = dict(instance_extra_items(value))
            state = {
                name: item
                for name, item in instance_items(value)
                if name not in slotnames and name not in extras
            }
            state.update(extras)
        else:
            state = dict(value.__dict__)
        if not state:
            state = None
    slots = {}
    for name in slotnames:
        try:
            slots[name] = getattr(value, name)
        except AttributeError:
            pass
    return (state, slots) if slots else state


@final
def _new_arguments(value):
    owner = type_of(value)
    missing = object()
    method = class_lookup_default(owner, "__getnewargs_ex__", missing)
    if method is not missing:
        result = bind_instance_attribute(method, value, owner)()
        if not isinstance(result, tuple):
            raise TypeError(
                f"__getnewargs_ex__ should return a tuple, not '{type(result).__name__}'"
            )
        if len(result) != 2:
            raise ValueError(
                f"__getnewargs_ex__ should return a tuple of length 2, not {len(result)}"
            )
        args, kwargs = result
        if not isinstance(args, tuple):
            raise TypeError(
                f"first item of the tuple returned by __getnewargs_ex__ must be a tuple, not '{type(args).__name__}'"
            )
        if not isinstance(kwargs, dict):
            raise TypeError(
                f"second item of the tuple returned by __getnewargs_ex__ must be a dict, not '{type(kwargs).__name__}'"
            )
        return args, kwargs
    method = class_lookup_default(owner, "__getnewargs__", missing)
    if method is missing:
        return (), {}
    args = bind_instance_attribute(method, value, owner)()
    if not isinstance(args, tuple):
        raise TypeError(
            f"__getnewargs__ should return a tuple, not '{type(args).__name__}'"
        )
    return args, {}


@final
def reduce(value, protocol):
    if protocol < 2:
        return copyreg._reduce_ex(value, protocol)
    args, kwargs = _new_arguments(value)
    owner = type_of(value)
    if kwargs:
        constructor, arguments = copyreg.__newobj_ex__, (owner, args, kwargs)
    else:
        constructor, arguments = copyreg.__newobj__, (owner,) + args
    state = value.__getstate__()
    listitems = iter(value) if isinstance(value, list) else None
    dictitems = iter(value.items()) if isinstance(value, dict) else None
    return constructor, arguments, state, listitems, dictitems


@final
def reduce_ex(value, protocol):
    protocol = _index_integer(protocol)
    if protocol < -(1 << 31) or protocol >= 1 << 31:
        raise OverflowError("Python int too large to convert to C int")
    try:
        method = value.__reduce__
    except AttributeError:
        pass
    else:
        if class_lookup(type_of(value), "__reduce__") is not class_lookup(
            object, "__reduce__"
        ):
            return method()
    return reduce(value, protocol)
