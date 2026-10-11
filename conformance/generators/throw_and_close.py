"""Generator throw() and close(): handling inside the generator, GeneratorExit, finally, and misuse errors."""

log = []


def resilient():
    while True:
        try:
            yield "ready"
        except ValueError as e:
            log.append("handled " + str(e))
            yield "recovered"


g = resilient()
print("start", next(g))
print("throw handled", g.throw(ValueError("bad")))
print("after recovery", next(g))
try:
    g.throw(KeyError("unhandled"))
except KeyError as e:
    print("throw unhandled propagates", type(e).__name__)
print("generator dead after unhandled", next(g, "dead"))
print("log", log)


def not_started():
    log.append("body ran")
    yield 1


log.clear()
g = not_started()
try:
    g.throw(RuntimeError("early"))
except RuntimeError as e:
    print("throw before start", type(e).__name__, log)
print("throw-before-start kills it", next(g, "dead"))


def with_finally():
    try:
        yield 1
        yield 2
    finally:
        log.append("finally ran")


log.clear()
g = with_finally()
next(g)
g.close()
print("close runs finally", log)
g.close()
print("second close is no-op", log)
print("closed generator", next(g, "closed"))

log.clear()
g = with_finally()
g.close()
print("close before start skips finally", log)


def catches_exit():
    try:
        yield 1
    except GeneratorExit:
        log.append("saw GeneratorExit")
        raise


log.clear()
g = catches_exit()
next(g)
g.close()
print("GeneratorExit observed", log)


def ignores_exit_once():
    try:
        yield 1
    except GeneratorExit:
        pass
    yield "ignored exit"  # yielding after GeneratorExit makes close() raise RuntimeError


log.clear()
g = ignores_exit_once()
next(g)
try:
    g.close()
except RuntimeError as e:
    print("yield after GeneratorExit", type(e).__name__)
g.close()
print("second close succeeds", next(g, "closed"))


def return_on_exit():
    try:
        yield 1
    except GeneratorExit:
        return "closing value"


g = return_on_exit()
next(g)
print("close returns value (3.13+)", g.close())


def throw_returns():
    try:
        yield 1
    except ValueError:
        return "returned after throw"


g = throw_returns()
next(g)
try:
    g.throw(ValueError)
except StopIteration as e:
    print("throw causing return", e.value)


def throw_type_and_instance():
    while True:
        try:
            yield
        except Exception as e:
            log.append(type(e).__name__ + ":" + str(e))


log.clear()
g = throw_type_and_instance()
next(g)
g.throw(ValueError)
g.throw(TypeError("instance"))
print("throw class vs instance", log)


def finally_yields_value():
    try:
        yield "body"
    finally:
        log.append("cleanup")


log.clear()
g = finally_yields_value()
next(g)
try:
    g.throw(IndexError("x"))
except IndexError:
    print("throw runs finally then propagates", log)


def nested_try():
    try:
        try:
            yield "inner"
        except ZeroDivisionError:
            log.append("inner caught")
            raise ArithmeticError("converted")
    except ArithmeticError as e:
        log.append("outer caught " + str(e))
        yield "outer"


log.clear()
g = nested_try()
next(g)
print("throw converted", g.throw(ZeroDivisionError), log)


def exception_context():
    try:
        yield
    except KeyError as e:
        yield type(e.__context__).__name__ if e.__context__ else "no context"


g = exception_context()
next(g)
try:
    raise ValueError("outer active")
except ValueError:
    print("thrown exception context", g.throw(KeyError("k")))


def exit_is_base_exception():
    try:
        yield
    except Exception:
        log.append("Exception caught GeneratorExit?")
    finally:
        log.append("finally")


log.clear()
g = exit_is_base_exception()
next(g)
g.close()
print("GeneratorExit not an Exception", log, issubclass(GeneratorExit, Exception))
