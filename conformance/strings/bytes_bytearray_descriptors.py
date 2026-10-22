"""Bytes, bytearray, and memoryview constructors, descriptors, mutation, and errors."""

b0 = bytes()
b1 = bytes(3)
b2 = bytes([65, 66, 67, 68])
b3 = bytes("hi", "utf-8")
b4 = bytes.fromhex("48 65 6c 6c 6f")
print("bytes_construct:", b0, b1, b2, b3, b4)
print("bytes_index_slice:", b2[0], b2[-1], b2[1:3], b2[::-1])
print("bytes_ops:", b"ab" + b"cd", b"xy" * 2, 66 in b2, b"BC" in b2, b"XZ" in b2)
print(
    "bytes_descriptors:",
    bytes.__len__(b2),
    bytes.__getitem__(b2, 1),
    bytes.__getitem__(b2, slice(1, 3)),
    bytes.decode(b4, "utf-8"),
    bytes.hex(b"\x0a\xff"),
    bytes.upper(b"abc"),
    bytes.lower(b"XYZ"),
    bytes.split(b"a,b,c", b","),
    bytes.join(b"-", [b"p", b"q"]),
)

ba = bytearray(b"abcd")
bytearray.__setitem__(ba, 0, 65)
bytearray.append(ba, 69)
bytearray.extend(ba, [70, 71])
popped = bytearray.pop(ba)
bytearray.remove(ba, 98)
bytearray.insert(ba, 1, 66)
print(
    "bytearray_mutation:",
    bytes(ba),
    popped,
    bytearray.__len__(ba),
    bytearray.__getitem__(ba, 0),
    bytearray.decode(ba, "utf-8"),
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
    memoryview.tobytes(mv_ro),
    memoryview.tolist(mv_ro),
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
    bytes.__len__(mb),
    isinstance(mba, bytearray),
    mba.total(),
    bytearray.__len__(mba),
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

for bad_receiver in (
    lambda: bytes.__len__("abc"),
    lambda: bytes.decode("abc"),
    lambda: bytearray.append(b"abc", 1),
    lambda: bytearray.__setitem__(b"abc", 0, 1),
):
    try:
        bad_receiver()
    except TypeError as e:
        print("receiver_error:", type(e).__name__)
