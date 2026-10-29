"""A class's __qualname__ lives on the type, outside its namespace."""


class Outer:
    class Inner:
        pass


print("qualname", Outer.Inner.__qualname__, "__qualname__" in vars(Outer.Inner))
Outer.Inner.__qualname__ = "Renamed"
print("renamed", Outer.Inner.__qualname__, repr(Outer.Inner))

Made = type("Made", (), {"__qualname__": "Somewhere.Made", "__module__": "here"})
print("type call", Made.__qualname__, sorted(vars(Made)))

try:
    type("Bad", (), {"__qualname__": 5})
except TypeError as error:
    print("bad namespace qualname", type(error).__name__, error)

try:
    Outer.__qualname__ = 3
except TypeError as error:
    print("bad assigned qualname", type(error).__name__, error)
print("after", Outer.__qualname__)
