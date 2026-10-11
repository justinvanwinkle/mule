"""global and nonlocal statements: multi-level nesting and global inside nested functions."""

g = "g0"


def set_global():
    global g
    g = "g1"


set_global()
print("set global", g)


def create_global():
    global brand_new
    brand_new = "created inside function"


create_global()
print("global created in function", brand_new)


def nested_global():
    g = "enclosing local"

    def inner():
        global g
        g = "g2 from inner"
        return g

    result = inner()
    return result, g


print("global inside nested function", nested_global(), g)


def counter():
    count = 0

    def bump():
        nonlocal count
        count += 1
        return count

    return bump


c1 = counter()
c2 = counter()
print("independent closures", c1(), c1(), c1(), c2())


def multi_level():
    value = "outer"

    def middle():
        def inner():
            nonlocal value
            value = "set by inner"

        inner()
        return value

    return middle(), value


print("nonlocal skips intermediate", multi_level())


def nearest_binding():
    value = "outermost"

    def middle():
        value = "middle"

        def inner():
            nonlocal value
            value = "inner rebinds middle"

        inner()
        return value

    return middle(), value


print("nonlocal binds nearest", nearest_binding())


def deep():
    a = 0

    def l1():
        def l2():
            def l3():
                def l4():
                    nonlocal a
                    a += 10
                    return a
                return l4()
            return l3()
        return l2()

    return l1(), l1(), a


print("deep nonlocal", deep())


# nonlocal and global in sibling functions sharing a name
shared = "global shared"


def siblings():
    shared = "enclosing shared"

    def use_global():
        global shared
        shared = "global changed"

    def use_nonlocal():
        nonlocal shared
        shared = "enclosing changed"

    use_global()
    use_nonlocal()
    return shared


print("siblings", siblings(), shared)


# global declaration in nested function with an enclosing variable of the same name
def global_skips_enclosing():
    target = "enclosing"

    def inner():
        global target
        return target

    return inner()


target = "module target"
print("global skips enclosing", global_skips_enclosing())


# nonlocal with multiple names and tuple assignment
def swapper():
    a, b = 1, 2

    def swap():
        nonlocal a, b
        a, b = b, a

    swap()
    return a, b


print("multi-name nonlocal", swapper())


# nonlocal modified through a loop and closure list
def accumulate():
    total = 0
    adders = []
    for i in range(3):
        def add(n=i):
            nonlocal total
            total += n
        adders.append(add)
    for add in adders:
        add()
    return total


print("accumulate", accumulate())


# global used in a class body method and class body itself
class_global = "before"


class Changer:
    global class_global
    class_global = "set in class body"

    def method(self):
        global class_global
        class_global = "set in method"


print("class body global", class_global)
Changer().method()
print("method global", class_global)


# nonlocal used in a method referring to enclosing function, not class
def class_in_function():
    state = "initial"

    class Inner:
        state = "class attribute"

        def change(self):
            nonlocal state
            state = "changed by method"

    Inner().change()
    return state, Inner.state


print("nonlocal from method", class_in_function())

# SyntaxErrors for invalid declarations, checked via compile
for source in ("def f():\n    nonlocal x\n",
               "nonlocal x\n",
               "def f(x):\n    global x\n",
               "def f():\n    x = 1\n    global x\n"):
    try:
        compile(source, "<test>", "exec")
        print("compiled", repr(source))
    except SyntaxError as e:
        print("syntax error", type(e).__name__)
