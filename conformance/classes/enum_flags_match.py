"""enum.Flag and IntFlag combinations, iteration over flag members, and enum members in match statements."""

from enum import Enum, Flag, IntFlag, auto


class Perm(Flag):
    R = auto()
    W = auto()
    X = auto()
    RW = R | W  # named alias combination


print("values", [(p.name, p.value) for p in Perm])
rw = Perm.R | Perm.W
print("combine", rw, rw is Perm.RW, rw.value)
rwx = Perm.R | Perm.W | Perm.X
print("unnamed combination", rwx, rwx.value, rwx.name)
print("contains", Perm.R in rwx, Perm.X in rw, Perm.RW in rwx)
print("iterate members of combo", [p.name for p in rwx])
print("len combo", len(rwx), len(Perm.R))
print("and/xor/invert", rwx & Perm.W, rwx ^ Perm.R, ~Perm.R, ~rwx)
empty = Perm(0)
print("empty", empty, bool(empty), bool(Perm.R), empty.name)
print("lookup by int", Perm(3), Perm(5))
try:
    Perm(8)
except ValueError as e:
    print("out of range Flag", type(e).__name__)


class Bits(IntFlag):
    A = 1
    B = 2
    C = 4


ab = Bits.A | Bits.B
print("IntFlag", ab, ab == 3, ab + 1, type(ab + 1).__name__, Bits.C | 8, type(Bits.C | 8).__name__)
print("IntFlag repr", repr(Bits.A | Bits.C))
print("IntFlag with int", Bits(7), Bits(9), int(Bits.B))
print("IntFlag bool", bool(Bits(0)), Bits.A & Bits.B)


class Color(Enum):
    RED = "r"
    GREEN = "g"
    BLUE = "b"


def classify(c):
    match c:
        case Color.RED:
            return "warm"
        case Color.GREEN | Color.BLUE:
            return "cool"
        case _:
            return "unknown"


print("match enum", [classify(c) for c in Color], classify("r"))


def perms(p):
    match p:
        case Perm.RW:
            return "read-write"
        case Perm.R:
            return "read"
        case x if Perm.X in x:
            return f"exec({x.name})"
        case _:
            return "other"


print("match flag", perms(Perm.R | Perm.W), perms(Perm.R), perms(Perm.X | Perm.R), perms(Perm.W))


def by_value(v):
    match Color(v):
        case Color.RED as red:
            return f"got {red.name}"
        case other:
            return f"other {other.value}"


print("match capture", by_value("r"), by_value("b"))


# Enum in class pattern with value attribute
def class_pattern(m):
    match m:
        case Color(value="g"):
            return "green via class pattern"
        case Color(name=n):
            return f"name {n}"


print("class pattern", class_pattern(Color.GREEN), class_pattern(Color.BLUE))


# Mapping patterns keyed by enum members
def mapping(d):
    match d:
        case {Color.RED: v}:
            return f"red key {v}"
        case _:
            return "no red"


print("mapping pattern", mapping({Color.RED: 1, Color.BLUE: 2}), mapping({Color.BLUE: 3}))


# Flag with boundary behavior
from enum import STRICT, CONFORM, KEEP


class Strict(Flag, boundary=STRICT):
    A = 1
    B = 2


class Conform(Flag, boundary=CONFORM):
    A = 1
    B = 2


class Keep(Flag, boundary=KEEP):
    A = 1
    B = 2


try:
    Strict(7)
except ValueError as e:
    print("STRICT", type(e).__name__)
print("CONFORM", Conform(7), Conform(7).value)
print("KEEP", Keep(7).value)

# Iterating whole Flag excludes aliases and zero
class WithZero(Flag):
    NONE = 0
    ONE = 1
    TWO = 2
    BOTH = 3


print("iter flag", [m.name for m in WithZero], list(WithZero.__members__))
