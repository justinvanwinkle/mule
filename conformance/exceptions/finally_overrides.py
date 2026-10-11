"""finally overriding with return/break/continue, swallowing exceptions and replacing return values."""


def return_in_finally_swallows():
    try:
        raise ValueError("lost")
    finally:
        return "finally return wins"


print("swallow by return:", return_in_finally_swallows())


def finally_overrides_try_return():
    try:
        return "try"
    finally:
        return "finally"


print("override return:", finally_overrides_try_return())


def finally_overrides_except_return():
    try:
        1 / 0
    except ZeroDivisionError:
        return "except"
    finally:
        return "finally"


print("override except return:", finally_overrides_except_return())


def return_value_evaluated_before_finally():
    x = 1
    try:
        return x
    finally:
        x = 99
        print("  finally sees x =", x)


print("return value captured early:", return_value_evaluated_before_finally())


def mutation_visible():
    items = [1]
    try:
        return items
    finally:
        items.append(2)


print("mutation in finally visible:", mutation_visible())


def break_swallows():
    log = []
    for i in range(3):
        try:
            log.append(i)
            raise KeyError(i)
        finally:
            break
    log.append("after loop")
    return log


print("break swallows:", break_swallows())


def continue_swallows():
    log = []
    for i in range(3):
        try:
            raise RuntimeError(i)
        finally:
            log.append(i)
            continue
    return log


print("continue swallows:", continue_swallows())


def break_overrides_return():
    while True:
        try:
            return "try return"
        finally:
            break
    return "after break"


print("break overrides return:", break_overrides_return())


def continue_overrides_return():
    count = 0
    for i in range(3):
        try:
            count += 1
            return "never kept"
        finally:
            continue
    return "loop finished " + str(count)


print("continue overrides return:", continue_overrides_return())


def nested_finally_returns():
    try:
        try:
            raise ValueError("inner")
        finally:
            return "inner finally"
    finally:
        print("  outer finally runs")


print("nested:", nested_finally_returns())


def finally_raise_replaces_return():
    try:
        return "try"
    finally:
        raise LookupError("from finally")


try:
    finally_raise_replaces_return()
except LookupError as e:
    print("finally raise replaces return:", e)


def finally_raise_replaces_exception():
    try:
        raise ValueError("original")
    finally:
        raise TypeError("replacement")


try:
    finally_raise_replaces_exception()
except TypeError as e:
    print("replacement", e, "context", repr(e.__context__))


def return_in_finally_swallows_base_exception():
    try:
        raise SystemExit(4)
    finally:
        return "SystemExit swallowed"


print(return_in_finally_swallows_base_exception())


def generator_finally():
    try:
        yield 1
        yield 2
    finally:
        print("  generator finally on close")


g = generator_finally()
print("gen first", next(g))
g.close()
print("gen closed")


def count_finally_runs():
    runs = []
    for i in range(3):
        try:
            try:
                if i == 0:
                    continue
                if i == 1:
                    raise ValueError
                return runs
            finally:
                runs.append("inner" + str(i))
        except ValueError:
            runs.append("caught" + str(i))
        finally:
            runs.append("outer" + str(i))
    return runs


print("finally runs:", count_finally_runs())
