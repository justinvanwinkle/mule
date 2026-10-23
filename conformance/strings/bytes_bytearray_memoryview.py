"""Bytes, bytearray, and memoryview constructors, indexing, slicing, methods, and mutation."""

b0 = bytes()
b1 = bytes(3)
b2 = bytes([65, 66, 67, 68])
b3 = bytes("hi", "utf-8")
b4 = bytes.fromhex("48 65 6c 6c 6f")
print("bytes_construct:", b0, b1, b2, b3, b4)
print("bytes_index_slice:", len(b2), b2[0], b2[-1], b2[1:3], b2[::-1])
print("bytes_ops:", b"ab" + b"cd", b"xy" * 2, 66 in b2, b"BC" in b2, b"XZ" in b2)
print(
    "bytes_methods:",
    b4.decode("utf-8"),
    b"\x0a\xff".hex(),
    b"  abc \n".strip(),
    b"a,b,c".split(b","),
    b"-".join([b"p", b"q"]),
    b"banana".replace(b"na", b"NA", 1),
    b"abracadabra".find(b"cad"),
    b"abracadabra".count(b"a"),
    b"foobar".startswith(b"foo"),
    b"foobar".endswith(b"bar"),
)

ba = bytearray(b"abcd")
ba[0] = 65
ba.append(69)
ba.extend([70, 71])
popped = ba.pop()
ba.remove(98)
ba.insert(1, 66)
print(
    "bytearray_mutation:",
    bytes(ba),
    popped,
    len(ba),
    ba[0],
    ba.decode("utf-8"),
)
ba[1:3] = b"XY"
del ba[0]
print("bytearray_slice_del:", bytes(ba))

mv_ro = memoryview(b"abcd")
print(
    "memoryview_ro:",
    len(mv_ro),
    mv_ro[0],
    mv_ro[-1],
    mv_ro[1:3].tobytes(),
    mv_ro.tobytes(),
    mv_ro.tolist(),
    mv_ro.hex(),
)

buf = bytearray(b"wxyz")
mv_rw = memoryview(buf)
mv_rw[0] = 87
mv_rw[2:4] = b"YZ"
print("memoryview_rw:", bytes(buf), mv_rw.tolist())
mv_rw.release()


class MyBytes(bytes):
    def tag(self):
        return b"tag:" + self


class MyByteArray(bytearray):
    def total(self):
        return sum(self)


mb = MyBytes(b"ok")
mba = MyByteArray(b"\x01\x02\x03")
mba.append(4)
print(
    "subclasses:",
    isinstance(mb, bytes),
    mb.tag(),
    len(mb),
    isinstance(mba, bytearray),
    mba.total(),
    len(mba),
)

for bad_value in (
    lambda: bytes([256]),
    lambda: bytes([-1]),
    lambda: bytearray([300]),
    lambda: bytes.fromhex("zz"),
):
    try:
        bad_value()
    except ValueError as e:
        print("value_error:", type(e).__name__)
