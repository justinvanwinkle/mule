"""f-string syntax: PEP 701 quote reuse, nesting, multi-line expressions, comments, backslashes, lambdas, walrus."""

name = "world"
items = {"a": 1, "b": [10, 20]}
print(f"basic {name} {1 + 2} {name.upper()} {items['a']} {items["b"][1]}")
print(f"same quotes {"nested"} {'single'} {"""triple"""}")
print(f"nested f-strings {f"{f"{name}"!r}"} {f'{1 + 1}' * 2}")
print(f"deep {f"{f"{f"{f"{1}"}"}"}"}")
print(f"backslash in expr {"\n".join(["x", "y"])!r} {'\t'.join("ab")}")
print(f"escapes outside {{braces}} \t| \N{BULLET} {'\N{SNOWMAN}'}")
print(f"conditional {'yes' if name else 'no'} {(lambda x: x * 2)(21)}")
print(f"walrus {(n := 5) + 1} then {n}")
print(f"dict literal {({'k': 'v'})['k']} set {sorted({3, 1, 2})}")
print(f"comprehension {[x * x for x in range(4)]} {"".join(c for c in "abc" if c != "b")}")
print(f"multi-line {
    sum(
        [1, 2, 3]  # a comment inside the expression
    )
} end")
print(f"""triple quoted
  {name!r:>10}
  {'''inner triple'''}""")
print(rf"raw f {name}\n", fr"\d{1 + 1}")
print(f"{name}" f"{name}" "plain" f"{{}}")
print(f"attribute {name.__class__.__name__} call {len(name)} slice {name[1:3]} {name[::-1]}")
print(f"star in expr {[*range(3), *'ab']}")
print(f"tuple without parens {1, 2}")
print(f"bool ops {0 or 'zero'} {None and 1} {not name}")
print(f"comparison {1 < 2 < 3} {name == 'world'}")
print(f"await-free generator {list(x for x in range(3))}")
print(f"empty str expr {''!r} {""!r}")
x = 10
print(f"{x}{x}{x}", f"{x=}{x}", f"{x:}", f"{x!s:}")


def gen():
    return f"in function {name} {x}"


print(gen())


class C:
    attr = "cls"

    def m(self):
        return f"{self.attr} {type(self).__name__}"


print(C().m())
print(f"evaluation order {[print('side', i) or i for i in range(2)]}")
counter = iter(range(100))
print(f"{next(counter)} {next(counter)} {next(counter)}")
print(f"unicode {'é'} {'😀'!a} {chr(0x41)}")
print(f"nested replacement in debug-like spot {'{'}{name}{'}'}")
print(F"capital F {x}")
