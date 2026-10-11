"""printf-style %-formatting for str: conversions, flags, width/precision, tuples, and mappings."""

print("s r a", "%s|%r|%a" % ("é'", "é'", "é'"))
print("d i u", "%d %i %u" % (42, -7, 3), "%d" % 3.99, "%d" % True)
print("o x X", "%o %x %X" % (8, 255, 255), "%#o %#x %#X" % (8, 255, 255), "%x" % -255)
print("e E f F", "%e %E" % (12345.678, 0.000123), "%f %F" % (1.5, float("inf")))
print("g G", "%g %g %g %G" % (0.0001, 0.00001, 1e16, 1e-10), "%#g" % 1.0)
print("c", "%c%c" % (65, "z"), "%c" % 0x1F600)
print("percent", "100%%", "%d%%" % 50)
print("width", "[%5d] [%-5d] [%05d] [%+d] [% d] [%+05d]" % (42, 42, 42, 42, 42, -42))
print("precision", "[%.2f] [%8.3f] [%-8.1f] [%.0f] [%.3s] [%.0e]" % (3.14159, 2.71828, 1.25, 2.5, "abcdef", 12345))
print("precision ints", "[%.3d] [%5.3d] [%#.3x]" % (7, -7, 10))
print("star width", "[%*d] [%-*d] [%.*f] [%*.*f]" % (6, 1, 6, 2, 3, 3.14159, 9, 2, 2.5))
print("tuple single", "%s" % ("only",), "%s" % ((1, 2),), "%s" % [1, 2])
print("mapping", "%(name)s is %(age)d years, %(name)r" % {"name": "Bo", "age": 7})
print("mapping width", "[%(x)5s] [%(y)-6.2f]" % {"x": "ab", "y": 2.0})
print("mapping %% only", "%(a)s %%" % {"a": 1})
print("big ints", "%d %x" % (2**70, 2**70), "%e" % 2**70)
print("neg zero", "%f %.1f %g" % (-0.0, -0.04, -0.0))
print("nan/inf", "%f %e %g %5.1f|" % (float("nan"), float("-inf"), float("inf"), float("nan")))
print("rounding", "%.2f %.2f %.0f %.0f" % (2.675, 1.005, 0.5, 1.5))
print("str of object", "%s %r" % (None, None), "%s" % b"x")


class Obj:
    def __str__(self):
        return "STR"

    def __repr__(self):
        return "REPR"

    def __index__(self):
        return 11


o = Obj()
print("custom obj", "%s %r %x" % (o, o, o))
try:
    print("custom %d", "%d" % o)
except TypeError as e:
    print("custom %d", type(e).__name__)
for fmt, args in [("%d", "x"), ("%s %s", ("a",)), ("%s", ("a", "b")), ("%(k)s", ("t",)), ("%z", (1,)), ("%c", (-1,)), ("%c", ("ab",)), ("%(k)s", {})]:
    try:
        print("error", repr(fmt), fmt % args)
    except Exception as e:
        print("error", repr(fmt), type(e).__name__)
print("dict as single arg", "%s" % {"k": 1})
print("no specifiers with mapping", "plain" % {})
print("length modifiers ignored", "%ld %hd %Lf" % (1, 2, 3.0))
