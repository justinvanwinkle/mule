"""Assignment expressions (walrus) in if/while conditions, comprehensions, and expressions."""

data = [3, 8, 0, 12, 5]

# if conditions
if (n := len(data)) > 3:
    print("long list", n)

if (first := data[0]) and (second := data[1]):
    print("both", first, second)

if (z := 0) or (w := "w"):
    print("or binding", z, w)

# while loop reading chunks
chunks = iter(["ab", "cd", "", "never"])
collected = []
while chunk := next(chunks):
    collected.append(chunk)
print("while walrus", collected)

# while with comparison
stack = [1, 2, 3, 4]
while (top := stack.pop()) != 2:
    print("popped", top)
print("stopped at", top, "stack", stack)

# comprehensions: walrus binds in enclosing scope
squares = [y for x in data if (y := x * x) > 20]
print("filtered squares", squares, "last y", y)

total = 0
running = [total := total + x for x in data]
print("running totals", running, "total", total)

# walrus in comprehension condition and value
pairs = [(x, half) for x in range(10) if (half := x // 2) % 2 == 0]
print("pairs", pairs, "half", half)

# nested comprehension binds outermost function scope
def nested():
    matrix = [[(last := i * j) for j in range(3)] for i in range(3)]
    return matrix, last


print("nested comp", nested())

# any() with walrus captures witness
if any((witness := x) > 10 for x in data):
    print("witness", witness)

if not all((counter := x) < 10 for x in data):
    print("counterexample", counter)

# walrus inside function call args, f-strings, and lambdas' surroundings
print("in call", max(a := 5, 3), a)
print(f"{(b := 'fstr')!r} {b}")
fn = lambda: (c := 7) + c
print("lambda walrus", fn())

# walrus in conditional expression
val = (m := 10) if (k := 3) > 2 else (m := 20)
print("ternary walrus", val, m, k)

# walrus in while with else
items = iter([1, 2, 3])
while (item := next(items, None)) is not None:
    if item == 99:
        break
else:
    print("while else item", item)

# walrus in a match subject and guard
match (subject := {"k": 1}):
    case {"k": got} if (doubled := got * 2) > 1:
        print("match walrus", subject, got, doubled)

# walrus inside nested function bodies is local to them
def outer():
    x = "outer"

    def inner():
        (x := "inner")
        return x

    return inner(), x


print("local walrus", outer())


# walrus combined with nonlocal in comprehension inside a closure
def counter_factory():
    count = 0

    def bump(values):
        nonlocal count
        return [count := count + v for v in values]

    return bump, lambda: count


bump, get = counter_factory()
print("bump", bump([1, 2, 3]), get())

# walrus evaluation order in tuple
t = ((p := 1), (p := p + 1), p)
print("tuple order", t)

# walrus in a generator expression body
g = ((q := x) for x in range(3))
print("gen", list(g), q)
