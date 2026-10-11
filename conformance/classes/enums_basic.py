"""enum.Enum, IntEnum, StrEnum: auto, iteration order, aliases, value/name lookup, comparisons, methods."""

from enum import Enum, IntEnum, StrEnum, auto, unique


class Color(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3
    CRIMSON = 1  # alias of RED

    def describe(self):
        return f"{self.name.lower()}={self.value}"

    @property
    def is_warm(self):
        return self is Color.RED

    @classmethod
    def default(cls):
        return cls.GREEN


print("repr/str", repr(Color.RED), str(Color.RED), f"{Color.BLUE}")
print("name/value", Color.GREEN.name, Color.GREEN.value)
print("iteration skips aliases", [m.name for m in Color])
print("members incl aliases", list(Color.__members__))
print("alias identity", Color.CRIMSON is Color.RED, Color.CRIMSON.name)
print("lookup by value", Color(2), Color(1))
print("lookup by name", Color["BLUE"], Color["CRIMSON"])
try:
    Color(9)
except ValueError as e:
    print("bad value", type(e).__name__)
try:
    Color["PINK"]
except KeyError as e:
    print("bad name", type(e).__name__)
print("methods", Color.RED.describe(), Color.RED.is_warm, Color.BLUE.is_warm, Color.default())
print("len", len(Color), "contains", Color.RED in Color, 1 in Color)
print("eq with value", Color.RED == 1, Color.RED != Color.GREEN)
try:
    Color.RED < Color.GREEN
except TypeError as e:
    print("Enum ordering", type(e).__name__)
print("hashable", {Color.RED: "r"}[Color.CRIMSON])
print("type", type(Color.RED).__name__, isinstance(Color.RED, Color))
try:
    Color.RED = 5
except AttributeError as e:
    print("reassign member", type(e).__name__)
try:
    Color.RED.value = 5
except AttributeError as e:
    print("set value", type(e).__name__)


class Auto(Enum):
    A = auto()
    B = auto()
    C = 10
    D = auto()


print("auto values", [(m.name, m.value) for m in Auto])


class Lower(Enum):
    @staticmethod
    def _generate_next_value_(name, start, count, last_values):
        return name.lower()

    X = auto()
    Y = auto()


print("custom auto", [m.value for m in Lower])

try:
    @unique
    class Dup(Enum):
        A = 1
        B = 1
except ValueError as e:
    print("unique", type(e).__name__)


class Priority(IntEnum):
    LOW = 1
    MID = 5
    HIGH = 10


print("IntEnum ops", Priority.LOW < Priority.HIGH, Priority.MID + 1, Priority.HIGH == 10, sorted([Priority.HIGH, Priority.LOW, 3]))
print("IntEnum str/format", str(Priority.MID), f"{Priority.MID}", repr(Priority.MID), format(Priority.MID, "03d"))
print("IntEnum index", [10, 20, 30][Priority.LOW], int(Priority.HIGH), isinstance(Priority.LOW, int))


class Mode(StrEnum):
    READ = auto()
    WRITE = "w"


print("StrEnum", Mode.READ, Mode.READ.value, Mode.WRITE == "w", Mode.WRITE.upper(), Mode("w").name)
print("StrEnum join", "+".join(Mode))


class Planet(Enum):
    MERCURY = (3.303e23, 2.4397e6)
    EARTH = (5.976e24, 6.37814e6)

    def __init__(self, mass, radius):
        self.mass = mass
        self.radius = radius

    @property
    def density_rank(self):
        return round(self.mass / self.radius ** 3 / 1e3, 3)


print("tuple-valued", Planet.EARTH.value, Planet.EARTH.mass, Planet.MERCURY.density_rank)

# Functional API
Animal = Enum("Animal", "CAT DOG BIRD")
print("functional", [(a.name, a.value) for a in Animal], Animal.DOG)
Shape = Enum("Shape", [("SQ", 4), ("TRI", 3)])
print("functional pairs", Shape(3), Shape.SQ.value)

# Enum with no members can be subclassed; with members cannot
class BaseEnum(Enum):
    def shout(self):
        return self.name + "!"


class Sub(BaseEnum):
    A = 1


print("subclass empty enum", Sub.A.shout())
try:
    class SubSub(Sub):
        B = 2
except TypeError as e:
    print("extend enum with members", type(e).__name__)

# Members are singletons and survive identity checks across lookups
print("singleton", Color(1) is Color["RED"] is Color.RED)

# _missing_ hook
class Status(Enum):
    OK = "ok"
    ERR = "err"

    @classmethod
    def _missing_(cls, value):
        if isinstance(value, str):
            for m in cls:
                if m.value == value.lower():
                    return m
        return None


print("_missing_", Status("OK"), Status("Err"))
try:
    Status(3)
except ValueError as e:
    print("_missing_ None", type(e).__name__)

# reversed and dir-free introspection
print("reversed", [m.name for m in reversed(Color)])
