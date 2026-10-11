"""try/except/else/finally execution order in every combination, including raises in else and finally."""


def run(raise_in_try, raise_in_else, raise_in_finally, handle):
    log = []
    try:
        try:
            log.append("try")
            if raise_in_try:
                raise ValueError("from try")
            log.append("try end")
        except ValueError as e:
            log.append("except " + str(e))
            if not handle:
                raise
        else:
            log.append("else")
            if raise_in_else:
                raise KeyError("from else")
            log.append("else end")
        finally:
            log.append("finally")
            if raise_in_finally:
                raise RuntimeError("from finally")
            log.append("finally end")
        log.append("after")
    except Exception as e:
        ctx = e.__context__
        log.append("outer " + type(e).__name__
                   + (" ctx=" + type(ctx).__name__ if ctx is not None else ""))
    return log


for t in (False, True):
    for el in (False, True):
        for fi in (False, True):
            for h in (False, True):
                print(f"try={t:d} else={el:d} finally={fi:d} handle={h:d}:", run(t, el, fi, h))


# try/finally without except.
def try_finally(fail):
    log = []
    try:
        try:
            log.append("body")
            if fail:
                1 / 0
        finally:
            log.append("cleanup")
    except ZeroDivisionError:
        log.append("propagated")
    return log


print("try/finally ok", try_finally(False))
print("try/finally fail", try_finally(True))


# An exception raised inside else is not handled by the same try's except clauses.
def else_not_covered():
    try:
        pass
    except ZeroDivisionError:
        return "wrong: except handled else-raise"
    else:
        try:
            1 / 0
        except ZeroDivisionError:
            return "inner handler in else"


print("else coverage", else_not_covered())

try:
    try:
        x = 1
    except KeyError:
        print("wrong")
    else:
        {}["else-key"]
except KeyError as e:
    print("else raise escapes to outer", repr(e.args[0]))


# Exception in except handler: finally still runs, new exception has context.
log = []
try:
    try:
        raise ValueError("first")
    except ValueError:
        log.append("handler")
        raise TypeError("second")
    finally:
        log.append("finally")
except TypeError as e:
    log.append("outer got " + str(e) + " ctx " + str(e.__context__))
print("handler raise", log)


# finally runs on loop break and continue.
log = []
for i in range(4):
    try:
        if i == 1:
            continue
        if i == 3:
            break
        log.append(i)
    finally:
        log.append("f" + str(i))
print("loop finally", log)
