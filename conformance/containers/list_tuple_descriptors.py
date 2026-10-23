"""List and tuple constructors, methods, descriptors, subclassing, and receiver checks."""

xs = list((10, 20, 30, 40))
print("list_construct:", list(), xs, len(xs), xs.__len__(), list.__len__(xs))
print(
    "list_getitem:",
    xs[0],
    xs[-1],
    xs[1:3],
    xs[::-1],
    list.__getitem__(xs, 2),
    list.__getitem__(xs, slice(0, 4, 2)),
)

list.__setitem__(xs, 1, 25)
list.__delitem__(xs, 2)
list.append(xs, 50)
list.extend(xs, (60, 70))
list.insert(xs, 0, 5)
popped = list.pop(xs, 2)
list.remove(xs, 60)
print("list_descriptors:", xs, popped, list.count(xs, 50), list.index(xs, 50))

copy_xs = list.copy(xs)
list.reverse(copy_xs)
print("list_reverse_copy:", copy_xs, xs)
list.sort(copy_xs, key=lambda n: -n)
print("list_sort_key:", copy_xs)
list.clear(copy_xs)
print("list_clear:", copy_xs)

self_ext = [1, 2, 3]
self_ext.extend(self_ext)
print("self_extend:", self_ext)

t = tuple([10, 20, 30, 20, 40])
print("tuple_construct:", tuple(), (1,), t, len(t), t.__len__(), tuple.__len__(t))
print(
    "tuple_descriptors:",
    t[0],
    t[-1],
    t[1:4],
    tuple.__getitem__(t, 3),
    tuple.__getitem__(t, slice(1, 5, 2)),
    tuple.count(t, 20),
    tuple.index(t, 20),
    tuple.index(t, 20, 2),
)
print(
    "comparisons:",
    [1, 2] == [1, 2],
    [1, 2] < [1, 3],
    (1, 2) < (1, 2, 0),
    hash((1, "a", (2, 3))) == hash((1, "a", (2, 3))),
)


class CustomList(list):
    def __getitem__(self, index):
        return list.__getitem__(self, index) * 10

    def append(self, value):
        list.append(self, value + 1)


cl = CustomList([1, 2, 3])
cl.append(10)
list.append(cl, 10)
print(
    "list_subclass:",
    isinstance(cl, list),
    cl[0],
    list.__getitem__(cl, 0),
    list(cl),
    list.__len__(cl),
)


class CustomTuple(tuple):
    def first(self):
        return tuple.__getitem__(self, 0)

    def __getitem__(self, index):
        return ("item", tuple.__getitem__(self, index))


ct = CustomTuple((7, 8, 9))
print(
    "tuple_subclass:",
    isinstance(ct, tuple),
    ct.first(),
    ct[1],
    tuple.__getitem__(ct, 1),
    tuple.__len__(ct),
)

for bad_index in (
    lambda: [][0],
    lambda: [].pop(),
    lambda: ()[0],
):
    try:
        bad_index()
    except IndexError as e:
        print("index_error:", type(e).__name__)

for bad_value in (
    lambda: [1, 2].remove(99),
    lambda: [1, 2].index(99),
    lambda: (1, 2).index(99),
):
    try:
        bad_value()
    except ValueError as e:
        print("value_error:", type(e).__name__)

for bad_receiver in (
    lambda: list.__len__(()),
    lambda: tuple.__len__([]),
    lambda: [1] + (2,),
    lambda: (1,) + [2],
):
    try:
        bad_receiver()
    except TypeError as e:
        print("type_error:", type(e).__name__)
