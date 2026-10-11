"""match sequence patterns: fixed length, star capture positions, nesting, strings, tuples vs lists."""

import array
import collections


def seq(v):
    match v:
        case []:
            return "empty"
        case [x]:
            return f"one {x!r}"
        case [x, y]:
            return f"two {x!r} {y!r}"
        case [first, *middle, last] if len(middle) < 3:
            return f"first {first!r} middle {middle} last {last!r}"
        case [*init, "end"]:
            return f"ends with 'end' after {init}"
        case ["start", *tail]:
            return f"starts with 'start' then {tail}"
        case [*_]:
            return "long sequence"
        case _:
            return "not a sequence: " + type(v).__name__


samples = [[], (), [1], (1,), [1, 2], (1, 2, 3), [1, 2, 3, 4, 5],
           ["a", "b", "c", "d", "end"], ["start", 1, 2, 3, 4, 5],
           list(range(10)), "ab", b"ab", bytearray(b"ab"), {"a": 1}, {1, 2},
           range(3), collections.deque([7, 8]), array.array("i", [1, 2]),
           memoryview(b"xy"), iter([1])]
for s in samples:
    label = type(s).__name__
    print("seq", label, "->", seq(s))


# star in the middle and at the start capture lists, even from tuples
def star_positions(v):
    match v:
        case (*head, 9):
            return "head", head, type(head).__name__
        case (0, *mid, 0):
            return "mid", mid
        case (1, 2, *tail):
            return "tail", tail
    return None


print("star head", star_positions((1, 2, 9)))
print("star mid", star_positions((0, 5, 6, 0)))
print("star mid empty", star_positions([0, 0]))
print("star tail", star_positions(range(1, 5)))


# list and tuple patterns both match any sequence
def bracket_kinds(v):
    match v:
        case (a, b):
            return "paren pattern", a, b
    return "no"


print("paren pattern vs list", bracket_kinds([1, 2]), bracket_kinds((1, 2)))


# distinguishing tuples from lists requires class patterns
def kind(v):
    match v:
        case tuple((a, b)):
            return f"tuple pair {a},{b}"
        case list([a, b]):
            return f"list pair {a},{b}"
        case _:
            return "other"


print("kind", kind((1, 2)), kind([1, 2]), kind("ab"))


# nested sequences
def nested(v):
    match v:
        case [[x, y], [z, *rest]]:
            return ("2x2", x, y, z, rest)
        case [(a, b), *pairs] if all(len(p) == 2 for p in pairs):
            return ("pairs", a, b, pairs)
        case [[[deep]]]:
            return ("deep", deep)
    return ("none",)


print("nested", nested([[1, 2], [3, 4, 5]]))
print("nested pairs", nested([(1, 2), (3, 4), [5, 6]]))
print("deep", nested([[[42]]]))
print("nested mismatch", nested([[1], [2]]))

# strings never match sequence patterns, even single char
match "x":
    case [c]:
        print("string matched as seq", c)
    case str(c):
        print("string matched as str", c)

# generators and iterators are not sequences
match (i for i in range(2)):
    case [a, b]:
        print("generator matched")
    case _:
        print("generator not a sequence")


# custom Sequence registered via collections.abc
class MySeq(collections.abc.Sequence):
    def __init__(self, *items):
        self.items = items

    def __len__(self):
        return len(self.items)

    def __getitem__(self, i):
        return self.items[i]


match MySeq("p", "q", "r"):
    case [first, *others]:
        print("custom Sequence", first, others)


# class with __getitem__/__len__ but not registered: not a sequence
class Ducky:
    def __len__(self):
        return 2

    def __getitem__(self, i):
        return [1, 2][i]


match Ducky():
    case [a, b]:
        print("duck matched")
    case _:
        print("duck not a sequence")

# star wildcard does not bind
match [1, 2, 3]:
    case [1, *_, last]:
        print("star wildcard last", last)

# length check before element matching (no element evaluated wrongly)
match [1, 2, 3]:
    case [1, 2]:
        print("wrong")
    case [_, _, _, *more]:
        print("three plus", more)
