"""Chained comparisons evaluate middle operands once and short-circuit."""

log = []


def v(name, value):
    log.append(name)
    return value


def show(label, value):
    print(label, value, log)
    log.clear()


show("1<2<3", v("a", 1) < v("b", 2) < v("c", 3))
show("short circuit", v("a", 3) < v("b", 2) < v("c", 1))
show("middle once", v("a", 1) < v("b", 5) > v("c", 4))
show("four terms", v("a", 1) <= v("b", 1) == v("c", 1) != v("d", 2))
show("four terms false early", v("a", 1) == v("b", 2) == v("c", 2) == v("d", 2))


class Rich:
    def __init__(self, n):
        self.n = n

    def __lt__(self, other):
        log.append(f"lt {self.n} {other.n}")
        return Result(self.n < other.n)

    def __repr__(self):
        return f"Rich({self.n})"


class Result:
    def __init__(self, value):
        self.value = value

    def __bool__(self):
        log.append(f"bool {self.value}")
        return self.value

    def __repr__(self):
        return f"Result({self.value})"


# chained comparison returns the last comparison result object, testing intermediates
show("rich chain true", Rich(1) < Rich(2) < Rich(3))
show("rich chain false", Rich(1) < Rich(0) < Rich(3))
show("single comparison not bool'd", Rich(1) < Rich(2))

# membership and identity chains
xs = [1, 2]
show("in chain", 1 in xs in [xs])
show("is chain", None is None is not False)
show("mixed in and <", 0 < 1 in [1, 2])
show("not in chain", 3 not in xs not in [[3]])

# chains with different operand types
show("float int chain", 1 < 1.5 < 2 < 2.5)
show("string chain", "a" < "b" < "c" < "ca")
show("tuple chain", (1, 2) < (1, 3) <= (1, 3))


# comparison across types raises
try:
    print(1 < "a" < 2)
except TypeError as e:
    print("mixed type", type(e).__name__)

# no error if short-circuited before bad comparison
show("short before error", v("a", 2) < v("b", 1) < v("c", "x"))

# chained comparisons in while conditions and comprehensions
i = 0
while 0 <= i < 3:
    i += 1
print("while chain", i)
print("comp chain", [n for n in range(10) if 2 < n <= 5])

# NaN chains
nan = float("nan")
print("nan chains", nan < 1 < 2, 1 < 2 > nan, nan == nan, nan != nan)


# == returning non-bool in chain
class Weird:
    def __eq__(self, other):
        log.append("eq")
        return "truthy-string"


show("eq returns non-bool", Weird() == 1)
show("eq chain", Weird() == 1 == 1)


# generator expression iterated in middle operand: evaluated once
def counter():
    counter.calls += 1
    return counter.calls


counter.calls = 0
print("middle called once", 0 < counter() < 10, counter.calls)
