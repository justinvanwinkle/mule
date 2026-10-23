"""For-loop iteration over builtin types, unpacking targets, and loop control."""

out_list = []
for x in [10, 20, 30]:
    out_list.append(x)
print("list:", out_list, x)

out_tuple = []
for x in ("a", "b", "c"):
    out_tuple.append(x)
print("tuple:", out_tuple)

out_str = []
for ch in "mule":
    out_str.append(ch)
print("str:", out_str)

out_bytes = []
for b in b"ABC":
    out_bytes.append(b)
print("bytes:", out_bytes)

out_bytearray = []
for b in bytearray(b"xyz"):
    out_bytearray.append(b)
print("bytearray:", out_bytearray)

d = {"k1": 1, "k2": 2, "k3": 3}
keys = []
for k in d:
    keys.append(k)
print("dict:", keys)

view_keys = []
for k in d.keys():
    view_keys.append(k)
print("dict_keys:", view_keys)

view_vals = []
for v in d.values():
    view_vals.append(v)
print("dict_values:", view_vals)

view_items = []
for k, v in d.items():
    view_items.append((k, v))
print("dict_items:", view_items)

set_acc = 0
for n in {2, 4, 6, 8}:
    set_acc += n
print("set_sum:", set_acc)

frozenset_acc = 1
for n in frozenset([2, 3, 5]):
    frozenset_acc *= n
print("frozenset_prod:", frozenset_acc)

r1 = []
for i in range(4):
    r1.append(i)
r2 = []
for i in range(2, 6):
    r2.append(i)
r3 = []
for i in range(10, 1, -3):
    r3.append(i)
r_empty = []
for i in range(5, 2):
    r_empty.append(i)
print("range:", r1, r2, r3, r_empty)

enum_items = []
for idx, val in enumerate(["p", "q", "r"]):
    enum_items.append((idx, val))
print("enumerate:", enum_items)

zip_items = []
for a, b, c in zip([1, 2, 3], ("x", "y"), "ab"):
    zip_items.append((a, b, c))
print("zip:", zip_items)

rev_items = []
for x in reversed([1, 2, 3, 4]):
    rev_items.append(x)
print("reversed:", rev_items)

map_items = []
for x in map(lambda n: n * n, [1, 2, 3, 4]):
    map_items.append(x)
print("map:", map_items)

filter_items = []
for x in filter(lambda n: n % 2 == 1, [1, 2, 3, 4, 5]):
    filter_items.append(x)
print("filter:", filter_items)

nested_unpack = []
for idx, (left, (mid, right)) in [(0, ("a", (1, 2))), (1, ("b", (3, 4)))]:
    nested_unpack.append((idx, left, mid, right))
print("nested_unpack:", nested_unpack)

starred_unpack = []
for head, *middle, tail in [(1, 2, 3, 4), (5, 6)]:
    starred_unpack.append((head, middle, tail))
print("starred_unpack:", starred_unpack)

control = []
for n in range(8):
    if n % 2 == 0:
        continue
    if n > 5:
        break
    control.append(n)
else:
    control.append(99)
print("break_skips_else:", control)

completed = []
for n in range(3):
    completed.append(n)
else:
    completed.append("done")
print("for_else:", completed)

matrix = []
for r in range(2):
    row = []
    for c in range(3):
        if r == 1 and c == 1:
            continue
        row.append(r * 10 + c)
    matrix.append(row)
print("nested_loops:", matrix)


class Countdown:
    def __init__(self, start):
        self.current = start

    def __iter__(self):
        return self

    def __next__(self):
        if self.current <= 0:
            raise StopIteration
        val = self.current
        self.current -= 1
        return val


custom_iter = []
for v in Countdown(3):
    custom_iter.append(v)
print("custom_iter:", custom_iter)


class SeqOnly:
    def __getitem__(self, index):
        if index >= 3:
            raise IndexError(index)
        return index + 100


seq_iter = []
for v in SeqOnly():
    seq_iter.append(v)
print("getitem_iter:", seq_iter)

for non_iterable in (42, None, True):
    try:
        for _ in non_iterable:
            pass
    except TypeError as e:
        print("non_iterable:", type(non_iterable).__name__, type(e).__name__)
