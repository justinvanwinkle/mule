"""while/else and for/else with break/continue, nested loops, and empty iterables."""


def find(items, target):
    for i, item in enumerate(items):
        if item == target:
            print("  found at", i)
            break
    else:
        print("  not found (else ran)")
        return -1
    return i


print("find hit", find([1, 2, 3], 2))
print("find miss", find([1, 2, 3], 9))
print("find empty", find([], 1))

# else runs for empty iterable
for x in []:
    print("never")
print("for else on empty list")

for x in range(0):
    pass
print("for else on range(0), x unbound:", "x" not in dir())

# while/else: else runs when condition becomes false, not on break
n = 0
while n < 3:
    n += 1
print("while else ran, n =", n)

n = 0
while n < 10:
    n += 1
    if n == 4:
        break
else:
    print("never printed")
print("while broke at", n)

while False:
    pass
print("while False else ran")

# continue does not prevent else
for i in range(4):
    if i % 2:
        continue
    print("even", i)
print("else after continue loop, i =", i)

# break in inner loop doesn't affect outer loop's else
for outer in range(3):
    for inner in range(3):
        if inner == 1:
            break
    else:
        print("inner else never")
    print("outer", outer, "inner stopped at", inner)
print("outer else ran")

# inner else runs; outer break triggered by inner else
for outer in "abc":
    for inner in range(2):
        if outer == "z":
            break
    else:
        if outer == "b":
            print("breaking outer from inner else at", outer)
            break
        continue
    print("never reaches here")
else:
    print("outer else never")

# primes classic
primes = []
for candidate in range(2, 30):
    for d in range(2, candidate):
        if candidate % d == 0:
            break
    else:
        primes.append(candidate)
print("primes", primes)

# else with return/raise inside loop body
def first_negative(xs):
    for x in xs:
        if x < 0:
            return x
    return None


print("first negative", first_negative([3, -1, -2]), first_negative([1, 2]))

# else clause in while that itself breaks out of an enclosing loop
attempts = 0
for round_no in range(5):
    k = 0
    while k < round_no:
        k += 1
        if k == 3:
            break
    else:
        attempts += 1
        continue
    print("while broke in round", round_no)
    break
print("attempts", attempts, "round", round_no)

# loop variable after loop
for last in range(5):
    pass
print("loop variable retains last value", last)

# else with generator that is exhausted immediately
def gen():
    return
    yield


for item in gen():
    print("never")
print("empty generator else")
