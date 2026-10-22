"""Str constructors, operators, descriptors, subclassing, and receiver checks."""

print("construct:", repr(str()), str(42), str(b"abc", "utf-8"))
s = "abcdef"
print("index_slice:", s[0], s[-1], s[1:4], s[::2], s[::-1], s[10:20])
print("ops:", "ab" + "cd", "xy" * 3, 2 * "z", "x" * 0, "abc" == "abc", "abc" < "abd")
print("contains:", "bcd" in s, "xyz" in s, "" in s, s.__contains__("cd"), str.__contains__(s, "cd"))
print("len:", len(s), s.__len__(), str.__len__(s), len(""), str.__len__(""))
print("getitem:", s.__getitem__(2), str.__getitem__(s, 2), str.__getitem__(s, slice(1, 5, 2)))

print(
    "descriptor_methods:",
    str.upper("hello"),
    str.lower("WORLD"),
    str.strip("  hi \n"),
    str.split("a,b,c", ","),
    str.join("-", ["p", "q", "r"]),
    str.replace("banana", "na", "NA", 1),
    str.startswith("foobar", "foo"),
    str.endswith("foobar", "bar"),
    str.find("abracadabra", "cad"),
    str.count("abracadabra", "a"),
    str.encode("hi", "utf-8"),
)


class MyStr(str):
    def shout(self):
        return str.upper(self) + "!"

    def __getitem__(self, index):
        return "<" + str.__getitem__(self, index) + ">"


ms = MyStr("abc")
print(
    "subclass:",
    isinstance(ms, str),
    ms.shout(),
    ms[1],
    str.__getitem__(ms, 1),
    str.upper(ms),
    str.__len__(ms),
)

try:
    _ = s[100]
except IndexError as e:
    print("index_error:", type(e).__name__)

try:
    _ = s[::0]
except ValueError as e:
    print("slice_step_zero:", type(e).__name__)

try:
    _ = 1 in s
except TypeError as e:
    print("contains_type_error:", type(e).__name__)

for bad_receiver in (
    lambda: str.__len__([1, 2]),
    lambda: str.upper(123),
    lambda: str.__getitem__(b"abc", 0),
    lambda: str.join(b"-", ["a", "b"]),
):
    try:
        bad_receiver()
    except TypeError as e:
        print("receiver_error:", type(e).__name__)
