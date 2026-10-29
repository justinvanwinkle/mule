"""Template string objects (PEP 750), compiled by Mule.

``t"..."`` evaluates to a ``Template`` of its literal strings and an
``Interpolation`` per replacement field, following CPython's
``Objects/templateobject.c`` and ``Objects/interpolationobject.c``.
"""

__speed__ = 3
__mule_builtin_implementation__ = True


class Interpolation:
    """One replacement field of a template string, already evaluated."""

    __match_args__ = ("value", "expression", "conversion", "format_spec")

    def __init__(self, value, expression="", conversion=None, format_spec=""):
        if conversion not in (None, "s", "r", "a"):
            raise ValueError("Interpolation() argument 'conversion' must be one of 's', 'a' or 'r'")
        self.value = value
        self.expression = expression
        self.conversion = conversion
        self.format_spec = format_spec

    def __reduce__(self):
        return Interpolation, (self.value, self.expression, self.conversion, self.format_spec)

    def __repr__(self):
        return (
            f"Interpolation({self.value!r}, {self.expression!r}, "
            f"{self.conversion!r}, {self.format_spec!r})"
        )


class Template:
    """The strings and interpolations of a template string literal."""

    def __init__(self, *args):
        strings = []
        interpolations = []
        pending = ""
        for arg in args:
            if isinstance(arg, str):
                pending = pending + arg
            elif isinstance(arg, Interpolation):
                strings.append(pending)
                pending = ""
                interpolations.append(arg)
            else:
                raise TypeError(
                    f"Template.__new__ *args need to be of type 'str' or "
                    f"'Interpolation', got {type(arg).__name__}"
                )
        strings.append(pending)
        self.strings = tuple(strings)
        self.interpolations = tuple(interpolations)

    @property
    def values(self):
        return tuple(interpolation.value for interpolation in self.interpolations)

    def __iter__(self):
        index = 0
        while index < len(self.interpolations):
            if self.strings[index]:
                yield self.strings[index]
            yield self.interpolations[index]
            index += 1
        if self.strings[index]:
            yield self.strings[index]

    def __add__(self, other):
        if not isinstance(other, Template):
            return NotImplemented
        return Template(*self, *other)

    def __reduce__(self):
        return Template, tuple(self)

    def __repr__(self):
        return f"Template(strings={self.strings!r}, interpolations={self.interpolations!r})"


def build_template(parts):
    return Template(*parts)
