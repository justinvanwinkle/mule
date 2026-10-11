"""memoryview over bytes and bytearray: indexing, slicing, conversion, mutation, cast, and release."""

data = b"hello"
mv = memoryview(data)
print("len", len(mv), "readonly", mv.readonly)
print("index", mv[0], mv[-1])
print("slice", bytes(mv[1:3]), mv[::2].tobytes(), mv[::-1].tobytes())
print("tobytes", mv.tobytes(), "tolist", mv.tolist())
print("format", mv.format, mv.itemsize, mv.ndim, mv.shape, mv.strides, mv.nbytes)
print("obj is source", mv.obj is data)
print("eq bytes", mv == b"hello", mv == b"hellx", mv[1:3] == b"el")
print("hex", mv.hex(), mv.hex(":"), mv[:2].hex("-", 1))
try:
    mv[0] = 72
except TypeError as e:
    print("readonly assign", type(e).__name__)
try:
    mv[10]
except IndexError as e:
    print("index out", type(e).__name__)

buf = bytearray(b"abcdef")
view = memoryview(buf)
view[0] = ord("A")
view[2:4] = b"CD"
print("mutate through view", buf, view.readonly)
sub = view[3:]
sub[0] = ord("x")
print("subview aliases", buf)
try:
    view[0:2] = b"xyz"
except ValueError as e:
    print("size mismatch", type(e).__name__)
try:
    view[0] = 256
except ValueError as e:
    print("byte range", type(e).__name__)
try:
    buf.append(1)
except BufferError as e:
    print("resize while exported", type(e).__name__)
view.release()
sub.release()
buf.append(ord("!"))
print("after release", buf)
try:
    view[0]
except ValueError as e:
    print("released access", type(e).__name__)

with memoryview(b"ctx") as ctx:
    print("context", ctx.tobytes())
try:
    len(ctx)
except ValueError as e:
    print("closed after with", type(e).__name__)

nums = bytearray([1, 0, 0, 0, 2, 0, 0, 0])
ints = memoryview(nums).cast("I")
print("cast", ints.format, ints.itemsize, len(ints), ints.tolist() in ([1, 2], [16777216, 33554432]))
back = ints.cast("B")
print("cast back", back.tolist())
print("cast shape", memoryview(b"abcdef").cast("B", (2, 3)).tolist())
print("bytes from view", bytes(memoryview(b"xyz")), bytearray(memoryview(b"q")))
print("iterate", list(memoryview(b"ab")), [b for b in memoryview(b"\x00\xff")])
print("in", 104 in memoryview(b"h"), b"h"[0] in memoryview(b"xyz"))
print("hash readonly", hash(memoryview(b"ab")) == hash(b"ab"))
try:
    hash(memoryview(bytearray(b"ab")))
except ValueError as e:
    print("hash writable", type(e).__name__)
try:
    memoryview("str")
except TypeError as e:
    print("str not buffer", type(e).__name__)
print("decode via bytes", bytes(memoryview("é".encode())).decode())
