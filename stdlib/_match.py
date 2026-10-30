"""Structural pattern matching checks, compiled by Mule.

A ``match`` statement compiles to tests over its subject; the checks here are
the parts CPython's ``Python/ceval.c`` implements natively: whether a subject
matches as a sequence or a mapping, and the attributes a class pattern reads.
"""

__speed__ = 3
__mule_builtin_implementation__ = True


_ABC = None


def _abc():
    global _ABC
    if _ABC is None:
        import _collections_abc

        _ABC = _collections_abc
    return _ABC


def sequence(subject):
    """The subject's items for a sequence pattern without a star capture, or None."""
    cls = type(subject)
    if cls is list or cls is tuple:
        return subject
    if subject is None or cls in _NON_SEQUENCE_TYPES:
        return None
    if isinstance(subject, (list, tuple)):
        return subject
    if isinstance(subject, (str, bytes, bytearray)):
        return None
    if isinstance(subject, _abc().Sequence):
        return list(subject)
    return None


def sequence_list(subject):
    """The subject's items as a list for a sequence pattern with a star capture, or None."""
    cls = type(subject)
    if cls is list or cls is tuple:
        return list(subject)
    if subject is None or cls in _NON_SEQUENCE_TYPES:
        return None
    if isinstance(subject, (list, tuple)):
        return list(subject)
    if isinstance(subject, (str, bytes, bytearray)):
        return None
    if isinstance(subject, _abc().Sequence):
        return list(subject)
    return None


def sequence_slice(values, before, after):
    cls = type(values)
    if cls is list:
        stop = len(values) - after if after else len(values)
        result = []
        index = before
        while index < stop:
            result.append(values[index])
            index += 1
        return result
    if cls is tuple:
        stop = len(values) - after if after else len(values)
        result = []
        index = before
        while index < stop:
            result.append(values[index])
            index += 1
        return result
    stop = len(values) - after if after else len(values)
    result = []
    index = before
    while index < stop:
        result.append(values[index])
        index += 1
    return result


_NON_SEQUENCE_TYPES = (
    dict,
    int,
    str,
    float,
    bool,
    bytes,
    bytearray,
    set,
    frozenset,
)

_NON_MAPPING_TYPES = (
    list,
    tuple,
    int,
    str,
    float,
    bool,
    bytes,
    bytearray,
    set,
    frozenset,
)


def mapping(subject):
    cls = type(subject)
    if cls is dict:
        return True
    if subject is None or cls in _NON_MAPPING_TYPES:
        return False
    if isinstance(subject, dict):
        return True
    return isinstance(subject, _abc().Mapping)


_MISSING = object()


def mapping_get(subject, key):
    if type(subject) is dict:
        return subject.get(key, _MISSING)
    return subject.get(key, _MISSING)


def mapping_values(subject, keys):
    """The values for ``keys``, or None when any key is absent."""
    seen = []
    values = []
    for key in keys:
        if key in seen:
            raise ValueError(f"mapping pattern checks duplicate key ({key!r})")
        seen.append(key)
        value = subject.get(key, _MISSING)
        if value is _MISSING:
            return None
        values.append(value)
    return values


def mapping_rest(subject, keys):
    rest = dict(subject)
    for key in keys:
        del rest[key]
    return rest


_SELF_MATCHING = (
    bool,
    bytearray,
    bytes,
    dict,
    float,
    frozenset,
    int,
    list,
    set,
    str,
    tuple,
)


def class_attributes(subject, cls, positional, keywords):
    """The attributes a class pattern matches its sub-patterns against, or
    None when the subject is not an instance or lacks one of them."""
    if not isinstance(cls, type):
        raise TypeError("called match pattern must be a class")
    if not isinstance(subject, cls):
        return None
    if not positional and not keywords:
        return ()
    if not keywords:
        match_args = getattr(cls, "__match_args__", None)
        if match_args is None:
            self_matching = False
            for builtin in _SELF_MATCHING:
                if issubclass(cls, builtin):
                    self_matching = True
            if not self_matching or positional > 1:
                raise TypeError(
                    f"{cls.__name__}() accepts 0 positional sub-patterns ({positional} given)"
                )
            return (subject,)
        if not isinstance(match_args, tuple):
            raise TypeError(f"{cls.__name__}.__match_args__ must be a tuple")
        if positional > len(match_args):
            raise TypeError(
                f"{cls.__name__}() accepts {len(match_args)} positional "
                f"sub-pattern{'s' if len(match_args) != 1 else ''} ({positional} given)"
            )
        values = []
        for i in range(positional):
            try:
                values.append(getattr(subject, match_args[i]))
            except AttributeError:
                return None
        return values
    names = []
    values = []
    if positional:
        match_args = getattr(cls, "__match_args__", None)
        if match_args is None:
            self_matching = False
            for builtin in _SELF_MATCHING:
                if issubclass(cls, builtin):
                    self_matching = True
            if not self_matching or positional > 1:
                raise TypeError(
                    f"{cls.__name__}() accepts 0 positional sub-patterns ({positional} given)"
                )
            values.append(subject)
        else:
            if not isinstance(match_args, tuple):
                raise TypeError(f"{cls.__name__}.__match_args__ must be a tuple")
            if positional > len(match_args):
                raise TypeError(
                    f"{cls.__name__}() accepts {len(match_args)} positional "
                    f"sub-pattern{'s' if len(match_args) != 1 else ''} ({positional} given)"
                )
            names.extend(match_args[:positional])
    for name in keywords:
        if name in names:
            raise TypeError(
                f"{cls.__name__}() got multiple sub-patterns for attribute {name!r}"
            )
        names.append(name)
    for name in names:
        try:
            values.append(getattr(subject, name))
        except AttributeError:
            return None
    return values
