"""match class patterns: __match_args__, keywords, builtins, dataclasses, inheritance."""

import dataclasses


class Point:
    __match_args__ = ("x", "y")

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return f"Point({self.x}, {self.y})"


class Point3(Point):
    __match_args__ = ("x", "y", "z")

    def __init__(self, x, y, z):
        super().__init__(x, y)
        self.z = z


class NoArgs:
    def __init__(self):
        self.a = 1


@dataclasses.dataclass
class Card:
    rank: int
    suit: str
    face_up: bool = False


@dataclasses.dataclass
class Hidden:
    shown: int
    secret: int = dataclasses.field(default=0, kw_only=True)


def where(p):
    match p:
        case Point3(0, 0, 0):
            return "3d origin"
        case Point3(x, y, z=zz):
            return f"3d {x} {y} {zz}"
        case Point(0, 0):
            return "origin"
        case Point(x=0, y=y):
            return f"on y axis at {y}"
        case Point(x, 0):
            return f"on x axis at {x}"
        case Point(x, y) if x == y:
            return "diagonal"
        case Point():
            return "somewhere"
        case _:
            return "not a point"


for p in (Point(0, 0), Point(0, 5), Point(3, 0), Point(2, 2), Point(1, 2),
          Point3(0, 0, 0), Point3(1, 2, 3), "point"):
    print("where", p, "->", where(p))


# builtin classes match self positionally
def builtin(v):
    match v:
        case bool(b):
            return f"bool {b}"
        case int(n) if n < 0:
            return f"negative int {n}"
        case int(n):
            return f"int {n}"
        case float(f):
            return f"float {f}"
        case str() if not v:
            return "empty str"
        case str(s):
            return f"str {s}"
        case bytes(bs):
            return f"bytes {bs!r}"
        case list([first, *_]):
            return f"list starting {first}"
        case list():
            return "empty list"
        case tuple(t):
            return f"tuple of {len(t)}"
        case dict({"k": k}):
            return f"dict with k={k}"
        case dict():
            return "other dict"
        case frozenset() | set():
            return "some set"
        case _:
            return "other"


for v in (True, -3, 4, 2.5, "", "hi", b"x", [9, 8], [], (1, 2), {"k": 1}, {}, {1}, frozenset(), None):
    print("builtin", repr(v), "->", builtin(v))

# dataclass auto __match_args__ excludes kw_only fields
print("Card match_args", Card.__match_args__, "Hidden match_args", Hidden.__match_args__)


def card(c):
    match c:
        case Card(1, suit):
            return f"ace of {suit}"
        case Card(rank, "hearts", face_up=True):
            return f"face-up heart {rank}"
        case Card(rank=r) if r > 10:
            return "court card"
        case Card():
            return "plain card"


print("cards", [card(c) for c in (Card(1, "spades"), Card(5, "hearts", True), Card(12, "clubs"), Card(3, "clubs"))])

match Hidden(1, secret=2):
    case Hidden(shown, secret=s):
        print("kw_only via keyword", shown, s)

try:
    match Hidden(1, secret=2):
        case Hidden(a, b):
            print("never")
except TypeError as e:
    print("too many positional", type(e).__name__)

# No __match_args__: positional patterns raise TypeError, keyword patterns work
match NoArgs():
    case NoArgs(a=1):
        print("keyword on no-match-args class")
try:
    match NoArgs():
        case NoArgs(1):
            pass
except TypeError as e:
    print("positional without match_args", type(e).__name__)

# missing attribute just fails to match
match Point(1, 2):
    case Point(z=_):
        print("never")
    case Point(x=1):
        print("missing attribute skips case")


# isinstance semantics: subclass matches base pattern; non-type raises
class Sub(Point):
    pass


print("subclass", where(Sub(0, 0)))
try:
    match 1:
        case len():
            pass
except TypeError as e:
    print("non-class in class pattern", type(e).__name__)


# __match_args__ that is not a tuple raises
class BadArgs:
    __match_args__ = ["x"]
    x = 1


try:
    match BadArgs():
        case BadArgs(1):
            pass
except TypeError as e:
    print("list match_args", type(e).__name__)


# attribute lookup goes through properties / __getattr__
class Lazy:
    __match_args__ = ("value",)

    def __getattr__(self, name):
        if name == "value":
            return "computed"
        raise AttributeError(name)


match Lazy():
    case Lazy("computed"):
        print("getattr-based attribute matched")

# nested class patterns
match [Point(1, Point(2, 3))]:
    case [Point(a, Point(b, c))]:
        print("nested class", a, b, c)
