"""pass, Ellipsis, docstring-only, and expression-statement bodies in every block kind."""


def only_pass():
    pass


def only_ellipsis():
    ...


def only_docstring():
    """Docstring is the whole body."""


class EmptyPass:
    pass


class EmptyEllipsis: ...


class OnlyDoc:
    """Doc."""


print("pass func", only_pass())
print("ellipsis func", only_ellipsis())
print("docstring func", only_docstring(), only_docstring.__doc__)
print("empty classes", EmptyPass.__name__, EmptyEllipsis.__name__, OnlyDoc.__doc__)
print("ellipsis value", ..., Ellipsis is ..., type(...).__name__, bool(...))

for _ in range(3):
    pass
print("pass loop var", _)

while False:
    ...

if True:
    pass
elif False:
    ...
else:
    pass

try:
    pass
except Exception:
    pass
else:
    ...
finally:
    pass

with open("pass_ellipsis_tmp.txt", "w"):
    pass
with open("pass_ellipsis_tmp.txt") as fh:
    print("empty file", repr(fh.read()))

match 5:
    case 1:
        pass
    case _:
        ...

lam = lambda: ...
print("lambda ellipsis", lam())


# methods with pass bodies, abstract-ish stubs
class Base:
    def hook(self):
        pass

    def stub(self) -> int: ...

    @property
    def prop(self):
        ...


b = Base()
print("method stubs", b.hook(), b.stub(), b.prop)

# expression statements are evaluated and discarded
log = []
log.append("a") or log.append("b")
[log.append(x) for x in "cd"]
1 + 2
"string statement"
print("expression statements", log)

# pass does not break or exit loops
count = 0
for i in range(5):
    if i == 2:
        pass
    count += 1
print("pass then continue", count)

# Ellipsis as a value in containers and comparisons
print("ellipsis in list", [1, ..., 3], ... in [1, ...], ... == Ellipsis)
print("ellipsis repr", repr(...), str(Ellipsis))
print("Ellipsis subscripting", {...: "dots"}[...])

# nested empty bodies
def deeply():
    if True:
        for _ in ():
            while True:
                pass
        else:
            ...
    return "done"


print("deeply", deeply())
