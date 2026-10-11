"""bytearray construction, mutation, slice assignment, aliasing, and methods."""

ba = bytearray(b"hello")
print("ctor", ba, bytearray(3), bytearray([1, 2]), bytearray("é", "utf-8"), bytearray())
ba[0] = 72
print("item assign", ba, ba[0])
ba[-1] = ord("O")
print("negative assign", ba)
for bad in [256, -1]:
    try:
        ba[0] = bad
    except ValueError as e:
        print("bad byte", bad, type(e).__name__)
try:
    ba[0] = b"x"
except TypeError as e:
    print("assign bytes to index", type(e).__name__)

ba[1:3] = b"EYY"
print("slice grow", ba)
ba[1:4] = b""
print("slice shrink", ba)
ba[::2] = b"__" if len(ba[::2]) == 2 else b"___"
print("extended slice assign", ba)
try:
    ba[::2] = b"toolong"
except ValueError as e:
    print("extended slice size", type(e).__name__)
del ba[0]
print("del index", ba)
ba = bytearray(b"0123456789")
del ba[::3]
print("del extended", ba)
ba[len(ba):] = b"!!"
print("append via slice", ba)

ba = bytearray(b"ab")
ba.append(99)
ba.extend(b"de")
ba.extend([102, 103])
ba.insert(0, 64)
ba.insert(-1, 35)
print("append/extend/insert", ba)
print("pop", ba.pop(), ba.pop(0), ba)
ba.remove(ord("c"))
print("remove", ba)
try:
    ba.remove(200)
except ValueError as e:
    print("remove missing", type(e).__name__)
ba.reverse()
print("reverse", ba)
c = ba.copy()
ba.clear()
print("copy and clear", c, ba, len(ba))

alias = bytearray(b"xyz")
other = alias
alias += b"!"
print("aliasing +=", other, other is alias)
alias *= 2
print("aliasing *=", other)
fresh = alias + b"?"
print("+ creates new", fresh, alias)

ba = bytearray(b"Hello World")
print("methods return bytearray", ba.upper(), type(ba.upper()).__name__, ba.split(), ba.replace(b"o", b"0"))
print("compare with bytes", ba == b"Hello World", bytes(ba), ba.hex(), bytearray.fromhex("4142"))
print("decode", ba.decode(), bytearray(b"\xc3\xa9").decode("utf-8"))
print("iteration", sum(ba), max(ba), ba.count(ord("o")), ba.find(b"World"))
try:
    hash(ba)
except TypeError as e:
    print("unhashable", type(e).__name__)
mv = memoryview(ba)
mv[0] = ord("J")
print("memoryview writes through", ba)
mv.release()
print("in-place translate", ba.translate(bytes.maketrans(b"lo", b"10")))
