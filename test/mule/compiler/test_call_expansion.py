"""Call argument expansion (``f(*iterable, **mapping)``) across operand kinds."""

from test.mule.compiler.test_exception_groups import compare


def test_star_arguments_expand_every_iterable_kind_in_source_order(tmp_path):
    compare(
        tmp_path,
        """
def show(*args, **kwargs):
    print(args, sorted(kwargs.items()))

class Tagged(tuple):
    def __iter__(self):
        yield "tagged"
        yield from tuple.__iter__(self)

class Counting(list):
    def __iter__(self):
        print("iterating list subclass")
        return list.__iter__(self)

def trace(label, value):
    print("evaluate", label)
    return value

show(*())
show(*(1, 2))
show(*[3, 4])
show(*"ab")
show(*range(3))
show(*(x * 2 for x in (1, 2)))
show(*{"k": 1}.keys())
show(*Tagged((5, 6)))
show(*Counting([7]))
show(trace("a", 1), *trace("b", (2, 3)), trace("c", 4), *trace("d", [5]))
show(*(1,), key=2)
show(*[1], **{"m": 3})
show(1, *(2,), **{"n": 4}, o=5)
print(*[1, 2], sep="-")
try:
    show(*5)
except TypeError as error:
    print(type(error).__name__)
try:
    show(*(1,), **{"k": 1}, k=2)
except TypeError as error:
    print(type(error).__name__)

def forward(*args):
    return show(*args)

forward()
forward(1)
forward(1, 2, 3)
args = (1, 2)
show(*args)
print(args)
""",
    )
