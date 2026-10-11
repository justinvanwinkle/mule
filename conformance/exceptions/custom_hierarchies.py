"""Custom exception hierarchies with __init__, extra attributes, and __str__/__repr__."""


class AppError(Exception):
    """Base for the application."""


class ConfigError(AppError):
    def __init__(self, key, reason):
        super().__init__(key, reason)
        self.key = key
        self.reason = reason

    def __str__(self):
        return f"config {self.key!r}: {self.reason}"


class MissingConfig(ConfigError, KeyError):
    def __init__(self, key):
        super().__init__(key, "missing")


class HttpError(AppError):
    def __init__(self, status, message="http failure"):
        self.status = status
        self.message = message
        super().__init__(message)

    def __repr__(self):
        return f"HttpError(status={self.status})"


class NotFound(HttpError):
    def __init__(self, path):
        super().__init__(404, "not found: " + path)
        self.path = path


class NoSuperInit(AppError):
    def __init__(self, a, b):
        self.total = a + b


class Wrapper(ValueError):
    pass


for exc in [ConfigError("port", "not an int"), MissingConfig("host"),
            HttpError(500), NotFound("/x"), NoSuperInit(2, 3), Wrapper("w", 1)]:
    try:
        raise exc
    except NotFound as e:
        print("NotFound", e.status, e.path, str(e), repr(e))
    except HttpError as e:
        print("HttpError", e.status, e.message, str(e), repr(e))
    except KeyError as e:
        print("KeyError branch", type(e).__name__, str(e), e.args, e.key)
    except ConfigError as e:
        print("ConfigError", e.key, e.reason, str(e), e.args)
    except NoSuperInit as e:
        # __init__ never forwarded args, but BaseException.__new__ stored them.
        print("NoSuperInit", e.total, e.args, repr(str(e)))
    except AppError as e:
        print("unexpected AppError", e)
    except ValueError as e:
        print("Wrapper", type(e).__name__, e.args, str(e), repr(e))

print("MissingConfig mro", [c.__name__ for c in MissingConfig.__mro__])
print("isinstance MissingConfig LookupError", isinstance(MissingConfig("k"), LookupError))


class Counted(Exception):
    created = 0

    def __init__(self, *args):
        Counted.created += 1
        self.serial = Counted.created
        super().__init__(*args)


for i in range(3):
    try:
        raise Counted("n", i)
    except Counted as e:
        print("Counted", e.serial, e.args)


# Raising the class instantiates it with no arguments.
try:
    raise Counted
except Counted as e:
    print("class raise serial", e.serial, e.args)


# Attributes can be added to any instance after construction.
try:
    err = ValueError("base")
    err.extra = {"line": 3}
    raise err
except ValueError as e:
    print("extra attr", e.extra, e)


# Exception subclass with class attribute default overridden per instance.
class Coded(Exception):
    code = 1

    def __init__(self, msg, code=None):
        super().__init__(msg)
        if code is not None:
            self.code = code


for c in (Coded("a"), Coded("b", 9)):
    try:
        raise c
    except Coded as e:
        print("Coded", e, e.code, "class code", Coded.code)


# Exception class defined inside a function is a fresh class per call.
def make():
    class Local(Exception):
        pass
    return Local


L1, L2 = make(), make()
try:
    raise L1("one")
except L2:
    print("wrong: caught by other local class")
except L1 as e:
    print("caught by its own local class", e)
