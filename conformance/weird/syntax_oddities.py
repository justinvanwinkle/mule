"""Legal-but-unusual layout: semicolons, one-line compound statements, backslash continuations, parenthesized with, deep nesting."""

import contextlib

a = 1; b = 2; c = a + b;
print("semicolons", a, b, c)

if a: print("one-line if"); print("second stmt on same line")
for i in range(3): print("one-line for", i, end=";"); print()
while a < 3: a += 1
print("one-line while", a)
def one_liner(x): return x * 2
class OneLine: attr = 5; other = attr + 1
print("one-line def/class", one_liner(4), OneLine.attr, OneLine.other)
try: raise ValueError("x")
except ValueError: print("one-line try/except")
finally: print("one-line finally")
with contextlib.nullcontext(9) as n: print("one-line with", n)

total = 1 + \
    2 + \
        3
print("backslash continuation", total)
if total > 5 and \
   total < 10:
    print("backslash in condition")
s = "abc" \
    "def"
print("backslash string concat", s)


@contextlib.contextmanager
def tag(name, log):
    log.append(f"<{name}>")
    yield name
    log.append(f"</{name}>")


log = []
with (
    tag("a", log) as x,
    tag("b", log) as y,
    tag("c", log),
):
    log.append(x + y)
print("parenthesized with", log)

with (tag("solo", log)):
    pass
with (tag("p", log)) as (z):
    pass
print("parenthesized single", log[-4:], z)

deep = ((((((((((1))))))))))
print("deep parens", deep)
nest = [[[[[[[[[["core"]]]]]]]]]]
print("deep brackets", nest[0][0][0][0][0][0][0][0][0][0])
mixed = {"k": [({"x": (1, [2, {3: (4,)}])},)]}
print("mixed nesting", mixed["k"][0][0]["x"][1][1][3][0])

long_expr = (1 + 2 - 3 * 4 // 5 % 6 + 7 ** 2 - (8 | 9) + (10 & 11) - (12 ^ 13) + (14 << 2) - (15 >> 1)
             + sum(range(16)) - len("seventeen") + max(18, 19, 20) - min(21, 22) + abs(-23) + int("24")
             - round(25.4) + (26 if 27 > 28 else 29) + [30, 31][True] - {32: 33}[32] + (lambda q: q)(34))
print("long expression", long_expr)

print  (  "spaces before call parens"  )
x  =  [  1 ,2 ,  3  ,  ]
print( x [ 0 ] , x [ - 1 ] , x [ :: - 1 ] )
print("tabs and spaces in expression",	1	+	2)
y = {
    'a':1,'b'  :  2,
        'c':
            3,
}
print("odd dict layout", y)


def f(
    a,
    /,
    b,
    *,
    c,
):
    return a, b, c


print("trailing comma params", f(
    1,
    2,
    c=3,
))
print("empty-ish statements"); pass; ...
if True:
    pass; print("pass then stmt")
lambda: (yield)
print("lambda containing yield is a generator", type((lambda: (yield))()).__name__)
print("number literals", 1_000_000, 0x_FF, 0o_17, 0b_1010, 1_0.5_0, 1e1_0, 0_0, 00)
print("string prefixes", rb"\n", Rb"\t", BR"x", f"{1}" "plain" rf"{2}\n")
print("implicit concat across lines", ("a"
                                       'b'
                                       """c"""
                                       f'{"d"}'))
print("ellipsis", ..., Ellipsis is ...)
print("comment after continuation", (1 +  # comment inside parens
                                     2))
if (True and
        False or
        True):
    print("multi-line condition")
print("conditional import inside function", (lambda: __import__("math").floor(2.5))())
print("unicode escapes", "\N{GREEK SMALL LETTER ALPHA}β\U0001F600"[0:2], len("\U0001F600"))
print("chained methods across lines", "  Hello  "
      .strip()
      .lower()
      .replace("l", "L"))
global_var = 1; del global_var; print("del on same line", "global_var" in globals())
