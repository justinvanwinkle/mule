"""Range and slice constructors, attributes, indexing, slicing, membership, and errors."""

r1 = range(5)
r2 = range(2, 10, 3)
r3 = range(10, 2, -2)
r_empty = range(5, 2)
print("range_attrs:", (r1.start, r1.stop, r1.step), (r2.start, r2.stop, r2.step), (r3.start, r3.stop, r3.step))
print("range_repr:", repr(r1), repr(r2), repr(r3), repr(r_empty))
print("range_len:", len(r1), len(r2), len(r3), len(r_empty))
print("range_index:", r2[0], r2[1], r2[-1], r3[0], r3[-1])

sub_r = r2[1:]
print("range_slice:", sub_r.start, sub_r.stop, sub_r.step, list(sub_r))
print("range_contains:", 5 in r2, 6 in r2, 8 in r2, 2 in r_empty)

s1 = slice(4)
s2 = slice(1, 5)
s3 = slice(1, 7, 2)
print("slice_attrs:", (s1.start, s1.stop, s1.step), (s2.start, s2.stop, s2.step), (s3.start, s3.stop, s3.step))
print("slice_repr:", repr(s1), repr(s2), repr(s3))

items = [10, 20, 30, 40, 50, 60, 70]
text = "abcdefg"
print("slice_apply:", items[s1], items[s2], items[s3], text[s3])

try:
    range(0, 10, 0)
except ValueError as e:
    print("zero_step:", type(e).__name__)

try:
    _ = r2[10]
except IndexError as e:
    print("range_index_error:", type(e).__name__)

try:
    hash(slice(1, 2, 3))
except TypeError as e:
    print("slice_unhashable:", type(e).__name__)
