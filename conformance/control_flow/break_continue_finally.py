"""break/continue/return passing through try/finally and with blocks; cleanup order."""

log = []


class Ctx:
    def __init__(self, name):
        self.name = name

    def __enter__(self):
        log.append("enter " + self.name)
        return self

    def __exit__(self, exc_type, exc, tb):
        log.append(f"exit {self.name} {exc_type.__name__ if exc_type else None}")
        return False


def flush(label):
    print(label, log)
    log.clear()


for i in range(3):
    try:
        if i == 1:
            continue
        if i == 2:
            break
        log.append(f"body {i}")
    finally:
        log.append(f"finally {i}")
flush("loop try/finally")

for i in range(3):
    with Ctx(f"c{i}"):
        if i == 0:
            continue
        log.append("after continue check")
        if i == 1:
            break
flush("loop with")


def ret_through():
    with Ctx("outer"):
        try:
            with Ctx("inner"):
                try:
                    return "value"
                finally:
                    log.append("inner finally")
        finally:
            log.append("outer finally")


print("returned", ret_through())
flush("return through nested")


def finally_overrides_return():
    try:
        return "try"
    finally:
        return "finally"


print("finally overrides", finally_overrides_return())


def finally_swallows_exception():
    for _ in range(2):
        try:
            raise ValueError("lost")
        finally:
            break
    return "swallowed"


print("break in finally", finally_swallows_exception())


def continue_in_finally():
    out = []
    for i in range(3):
        try:
            if i == 1:
                raise KeyError(i)
            out.append(i)
        finally:
            continue
    return out


print("continue in finally", continue_in_finally())


def return_value_evaluated_before_finally():
    x = [1]
    try:
        return x
    finally:
        x.append(2)
        log.append("finally mutated")


print("returned object mutated by finally", return_value_evaluated_before_finally())
flush("mutation log")


def rebinding_in_finally():
    x = 1
    try:
        return x
    finally:
        x = 2


print("rebinding in finally does not change return", rebinding_in_finally())

# while loop with break inside try/except/else/finally
n = 0
while True:
    n += 1
    try:
        if n == 3:
            break
        if n == 1:
            raise ValueError
    except ValueError:
        log.append(f"except {n}")
        continue
    else:
        log.append(f"else {n}")
    finally:
        log.append(f"finally {n}")
flush("while try/except/else/finally")

# with __exit__ that suppresses exceptions, combined with continue
class Suppress(Ctx):
    def __exit__(self, exc_type, exc, tb):
        super().__exit__(exc_type, exc, tb)
        return True


for i in range(3):
    with Suppress(f"s{i}"):
        if i == 1:
            raise RuntimeError
        log.append(f"body {i}")
    log.append(f"after with {i}")
flush("suppressing with")

# generator close runs finally
def gen():
    try:
        for i in range(10):
            yield i
    finally:
        log.append("gen finally")


running = gen()
for v in running:
    if v == 2:
        break
log.append("loop exited, generator still suspended")
running.close()
flush("break out of generator loop then close")

g = gen()
next(g)
g.close()
flush("explicit close")

# nested loops with finally in each level
for a in range(2):
    try:
        for b in range(3):
            try:
                if b == 1:
                    break
                log.append(f"a{a}b{b}")
            finally:
                log.append(f"inner fin a{a}b{b}")
    finally:
        log.append(f"outer fin a{a}")
flush("nested finally")
