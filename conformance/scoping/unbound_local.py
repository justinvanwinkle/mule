"""UnboundLocalError from later assignments, deletes, and other local-making statements."""

counter = 0
name = "global name"


def attempt(label, func):
    try:
        print(label, func())
    except UnboundLocalError as e:
        print(label, type(e).__name__, isinstance(e, NameError))
    except NameError as e:
        print(label, type(e).__name__)


def increment():
    counter += 1
    return counter


def read_then_assign():
    value = name
    name = "local"
    return value


def assign_after_return():
    return name
    name = "never"


def augmented_in_branch():
    if False:
        name = 1
    return name


def deleted_local():
    name = "temp"
    del name
    return name


def for_target_later():
    first = name
    for name in range(1):
        pass
    return first


def import_later():
    result = math
    return result


def def_later():
    result = helper
    def helper():
        pass
    return result


def class_later():
    result = Klass
    class Klass:
        pass
    return result


def except_as_later():
    result = err
    try:
        pass
    except Exception:
        pass
    return result


def with_as_later():
    result = fh
    with open("unbound_tmp.txt", "w") as fh:
        pass
    return result


def walrus_later():
    result = w
    if (w := 1):
        pass
    return result


def match_capture_later():
    result = captured
    match 1:
        case captured:
            pass
    return result


attempt("augmented assignment to global name", increment)
attempt("read then assign", read_then_assign)
attempt("assignment after return", assign_after_return)
attempt("assignment in dead branch", augmented_in_branch)
attempt("read after del", deleted_local)
attempt("for target later", for_target_later)
attempt("import later", import_later)
attempt("def later", def_later)
attempt("class later", class_later)
attempt("except-as later", except_as_later)
attempt("with-as later", with_as_later)
attempt("walrus later", walrus_later)
attempt("match capture later", match_capture_later)


# enclosing variable referenced before assignment in the enclosing function
def free_before_assignment():
    def inner():
        return later

    try:
        inner()
    except NameError as e:
        first = type(e).__name__
    later = "assigned"
    return first, inner()


print("free variable before assignment", free_before_assignment())


# free variable deleted in enclosing scope
def free_deleted():
    v = 1

    def inner():
        return v

    del v
    try:
        return inner()
    except NameError as e:
        return type(e).__name__


print("free variable deleted", free_deleted())


# the fix: global/nonlocal declarations
def fixed_increment():
    global counter
    counter += 1
    return counter


print("global fix", fixed_increment(), fixed_increment())


# comprehension does not make the name local to the function
def comprehension_does_not_bind():
    items = [name for name in "ab"]
    return name, items


print("comprehension leaves outer resolution", comprehension_does_not_bind())


# class body: unbound name falls back to global instead of raising
class Body:
    name = name + " (via class body)"


print("class body reads global then binds", Body.name)
