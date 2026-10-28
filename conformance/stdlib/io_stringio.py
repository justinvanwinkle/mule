"""io.StringIO: accumulation, overwrite, overseek padding, newline modes, readline limits, truncate, iteration, state, errors."""
import io
import pickle


def error(action):
    try:
        action()
    except Exception as e:
        return type(e).__name__ + ": " + str(e)
    return "no error"


s = io.StringIO()
for piece in ["alpha ", "", "beta ", "gamma\n", "éè\u20ac"]:
    print("write", s.write(piece), s.tell())
print("value", repr(s.getvalue()), s.tell())
print("value again", repr(s.getvalue()))
print("read at end", repr(s.read()))
print("seek", s.seek(6), repr(s.read(4)), s.tell())
print("overwrite", s.write("BETA"), repr(s.getvalue()), s.tell())
print("seek end", s.seek(0, 2), s.write("!"), repr(s.getvalue()))
print("seek cur", s.seek(0, 1))
print("overseek", s.seek(30), s.write("x"), repr(s.getvalue()), len(s.getvalue()))
print("read past", repr(s.read(5)), s.tell())
print("truncate", s.truncate(8), repr(s.getvalue()), s.tell())
s.seek(3)
print("truncate default", s.truncate(), repr(s.getvalue()), s.tell())
print("truncate larger", s.truncate(100), repr(s.getvalue()))
print("read sizes", s.seek(0), repr(s.read(None)), s.seek(0), repr(s.read(-5)), s.seek(1), repr(s.read(0)), repr(s.read(1)))

lines = io.StringIO("one\ntwo\r\nthree\rfour\n\nlast")
print("readline", repr(lines.readline()), repr(lines.readline(2)), repr(lines.readline()), repr(lines.readline(None)))
print("readlines", lines.readlines())
lines.seek(0)
print("iterate", list(lines))
lines.seek(0)
print("readlines hint", lines.readlines(5))

for newline in [None, "", "\n", "\r", "\r\n"]:
    text = io.StringIO(newline=newline)
    text.write("a\nb\r\nc\rd")
    text.write("\r")
    text.write("\ne\n")
    print("newline", repr(newline), repr(text.getvalue()), text.tell(), text.newlines)
    text.seek(0)
    print("  lines", text.readlines())
    initial = io.StringIO("x\r\ny\rz\n", newline=newline)
    print("  initial", repr(initial.getvalue()), initial.tell(), repr(initial.readline()))

print("none initial", repr(io.StringIO(None).getvalue()))
with io.StringIO("ctx") as managed:
    print("context", managed.read(), managed.closed)
print("closed", managed.closed)
for name, action in [
    ("read closed", lambda: managed.read()),
    ("write closed", lambda: managed.write("x")),
    ("getvalue closed", lambda: managed.getvalue()),
    ("tell closed", lambda: managed.tell()),
    ("next closed", lambda: next(managed)),
    ("write bytes", lambda: io.StringIO().write(b"x")),
    ("bad initial", lambda: io.StringIO(b"x")),
    ("bad newline", lambda: io.StringIO(newline="x")),
    ("newline type", lambda: io.StringIO(newline=1)),
    ("negative seek", lambda: io.StringIO().seek(-1)),
    ("relative seek", lambda: io.StringIO("abc").seek(1, 1)),
    ("bad whence", lambda: io.StringIO().seek(0, 3)),
    ("float seek", lambda: io.StringIO().seek(1.0)),
    ("negative truncate", lambda: io.StringIO().truncate(-1)),
    ("read float", lambda: io.StringIO("ab").read(1.5)),
    ("detach", lambda: io.StringIO().detach()),
]:
    print(name, error(action))

flags = io.StringIO()
print("flags", flags.readable(), flags.writable(), flags.seekable(), flags.isatty(), flags.line_buffering, flags.encoding, flags.errors, flags.newlines)
print("isinstance", isinstance(flags, io.TextIOBase), isinstance(flags, io.IOBase))
flags.writelines(["p", "q"])
print("writelines", repr(flags.getvalue()))
print("print file", print(1, "two", sep="-", file=flags), repr(flags.getvalue()))


class Sub(io.StringIO):
    def readline(self, size=-1):
        return super().readline(size).upper()


sub = Sub("ab\ncd\n")
print("subclass iterate", list(sub))
sub.extra = 5
clone = pickle.loads(pickle.dumps(sub))
print("pickle", type(clone).__name__, repr(clone.getvalue()), clone.tell(), clone.extra)
plain = io.StringIO("q\r\nr", newline=None)
plain.seek(1)
copy = pickle.loads(pickle.dumps(plain))
print("pickle newline", repr(copy.getvalue()), copy.tell(), repr(copy.read()))

big = io.StringIO()
for index in range(20000):
    big.write(str(index))
    big.write(",")
value = big.getvalue()
print("big", len(value), value[:20], value[-12:], big.tell())
