"""Private name mangling across subclasses, nested functions, getattr strings, and classes with leading underscores."""


class Base:
    def __init__(self):
        self.__secret = "base"
        self.__dunder__ = "not mangled"

    def base_peek(self):
        return self.__secret

    def __hidden(self):
        return "base hidden"

    def call_hidden(self):
        return self.__hidden()


class Child(Base):
    def __init__(self):
        super().__init__()
        self.__secret = "child"

    def child_peek(self):
        return self.__secret

    def __hidden(self):
        return "child hidden"


c = Child()
print("separate slots", c.base_peek(), c.child_peek())
print("mangled attrs", sorted(k for k in vars(c) if "secret" in k))
print("dunder not mangled", c.__dunder__)
print("call_hidden uses Base's", c.call_hidden())
print("explicit mangled access", c._Base__secret, c._Child__secret)
print("getattr string not mangled", c._Base__secret, hasattr(c, "__secret"))


class Nested:
    def method(self):
        __local = "locals mangled too"

        def inner():
            return __local

        def make_attr(obj):
            obj.__made = 1
            return sorted(vars(obj))

        lam = lambda: self.__thing
        self.__thing = "via lambda"
        comp = [self.__thing for _ in range(1)]
        return inner(), make_attr(self), lam(), comp


print("nested functions", Nested().method())


class _Leading:
    def __init__(self):
        self.__x = 1


print("single leading underscore class", sorted(vars(_Leading())))


class __Double:
    def __init__(self):
        self.__x = 2


print("double leading underscore class", sorted(vars(__Double())))


class ___:
    def __init__(self):
        self.__x = 3


print("all underscores class: no mangling", sorted(vars(___())))


# Mangling applies to names in global statements and imports in class bodies
__module_private = "module-level"


class UsesGlobal:
    def get(self):
        try:
            return __module_private
        except NameError as e:
            return type(e).__name__


_UsesGlobal__module_private = "mangled global"
print("global name mangled in method", UsesGlobal().get())


# Keyword argument names in calls are not mangled
def takes(**kw):
    return sorted(kw)


class KW:
    def call(self):
        return takes(__k=1, normal=2)


print("kwarg names not mangled", KW().call())


# Class attributes and class-level references
class Attr:
    __shared = "shared"
    names = [k for k in dir() if "shared" in k]

    @classmethod
    def get(cls):
        return cls.__shared


print("class attr mangled", Attr.get(), "_Attr__shared" in vars(Attr), Attr.names)


# Mangling with setattr/delattr/hasattr uses literal strings
class Dyn:
    def go(self):
        setattr(self, "__raw", 1)
        self.__cooked = 2
        return sorted(vars(self)), hasattr(self, "__cooked"), hasattr(self, "_Dyn__cooked")


print("dynamic", Dyn().go())


# Subclass trying to read parent's private
class Peeker(Base):
    def peek(self):
        try:
            return self.__secret
        except AttributeError as e:
            return type(e).__name__


print("subclass cannot see parent private", Peeker().peek())


# Name with trailing double underscore is not mangled; one trailing underscore is
class Trailing:
    def __init__(self):
        self.__a_ = 1
        self.__b__ = 2


print("trailing underscores", sorted(vars(Trailing())))
