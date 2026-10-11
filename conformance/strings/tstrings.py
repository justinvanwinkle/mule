"""PEP 750 template strings: Template/Interpolation structure, iteration, and a small renderer."""

from string.templatelib import Interpolation, Template

name = "World"
amount = 1234.5678
tmpl = t"Hello {name}! You owe {amount:,.2f} ({name!r})."
print("type", type(tmpl).__name__, isinstance(tmpl, Template))
print("strings", tmpl.strings)
print("values", tmpl.values)
for i in tmpl.interpolations:
    print("interp", repr(i.value), repr(i.expression), repr(i.conversion), repr(i.format_spec), isinstance(i, Interpolation))

print("iteration:")
for part in tmpl:
    if isinstance(part, str):
        print("  str", repr(part))
    else:
        print("  interp", part.expression, repr(part.value))

empty = t""
print("empty", empty.strings, empty.interpolations, list(empty))
only = t"{name}"
print("only interp strings", only.strings, [type(p).__name__ for p in only])
adjacent = t"{1}{2}"
print("adjacent", adjacent.strings, adjacent.values, list(adjacent) == list(adjacent.interpolations))
expr = t"{name.upper() + '!'} {[x * 2 for x in range(3)]} {amount = :.1f}"
print("expressions", [i.expression for i in expr.interpolations], expr.values)
print("debug strings", expr.strings)
w = 8
nested = t"{amount:{w}.{2}f}|"
print("nested spec", repr(nested.interpolations[0].format_spec))
print("conversions", [(i.expression, i.conversion) for i in t"{name!s}{name!a}{name}".interpolations])


def convert(value, conversion):
    if conversion == "r":
        return repr(value)
    if conversion == "s":
        return str(value)
    if conversion == "a":
        return ascii(value)
    return value


def render(template):
    out = []
    for part in template:
        if isinstance(part, str):
            out.append(part)
        else:
            out.append(format(convert(part.value, part.conversion), part.format_spec))
    return "".join(out)


def html_escape_render(template):
    out = []
    for part in template:
        if isinstance(part, Interpolation):
            text = render(Template(part)) if isinstance(part.value, Template) else str(part.value)
            out.append(text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))
        else:
            out.append(part)
    return "".join(out)


print("render", render(tmpl))
print("render matches f-string", render(tmpl) == f"Hello {name}! You owe {amount:,.2f} ({name!r}).")
print("render nested spec", render(nested))
user = "<script>&"
print("html render", html_escape_render(t"<p>{user}</p>"))

built = Template("a", Interpolation(1, "one"), "b", Interpolation(2, "two", "r", ">3"))
print("constructed", built.strings, built.values, render(built))
print("constructed adjacent strs merge", Template("x", "y").strings)
print("constructed interp attrs", Interpolation(5, "five").conversion, repr(Interpolation(5, "five").format_spec))
combined = t"left {name} " + t"right {amount:.0f}"
print("concat templates", combined.strings, combined.values, render(combined))
try:
    t"x" + "y"
except TypeError as e:
    print("template + str", type(e).__name__)
print("implicit concat", (t"a{1}" t"b{2}").strings)
v1, v2 = 1, 2
i = t"{v1}".interpolations[0]
print("interp match", end=" ")
match i:
    case Interpolation(value, expression, conversion, format_spec):
        print(value, expression, conversion, repr(format_spec))
lazy = [t"{k}" for k in range(3)]
print("captured eagerly", [tt.values for tt in lazy])
print("raw t-string", rt"\n{name}".strings, tr"\d".strings)
