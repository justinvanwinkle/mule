"""f-string debug specifier `=`, conversions, and nested replacement fields in format specs."""

x = 3.14159
w = 10
p = 3
s = "hi"
n = 255

print(f"{x=}")
print(f"{x = }")
print(f"{x =}", f"{x= }")
print(f"{x=:.2f}")
print(f"{x = :>{w}.{p}f}")
print(f"{s=}", f"{s=!s}", f"{s=!r}", f"{s=!a}", f"{'é'=}")
print(f"{s=:>5}", f"{s=!r:>8}")
print(f"{n + 1=}", f"{n*2 = }", f"{[n, s]=}")
print(f"{len(s)=} {s.upper()=}")
print(f"{x:{w}.{p}f}|")
print(f"{x:{'<'}{w}}|", f"{s:{'*'}^{w}}")
print(f"{n:{'#x'}}", f"{n:{'b'}}", f"{n:#{w}_x}")
fmt = ".1e"
print(f"{x:{fmt}}", f"{x:{fmt.upper()}}")
print(f"{x:{w}}|{x:<{w}}|{x:^{w}.{p - 1}}|")
print(f"{n:{w},}", f"{10**7:{'_'}}")
width = 6
for item in ["a", "bb", "ccc"]:
    print(f"[{item:>{width}}] [{item:{'-'}<{width + 1}}]")
print(f"{'nested ' + f'{x:.{p}f}'}")
print(f"{x!r:>20}", f"{s!r:^9}")
d = {"key": 7}
print(f"{d['key']=}", f"{d["key"]:03d}")
print(f"{'='}", f"{'!='}", f"{1 != 2=}", f"{(lambda: 'lam')()=}")
print(f"{x=!s:.3}")
v = None
print(f"{v=}", f"{v!s:>6}|")
print(f"{3=}", f"{3 == 3=}")
print(f"""{
    x
    =}""")
print(f"{x:.{p}}", f"{x:.{p}%}", f"{0.5:{w}.{p}%}")
print(f"{n:{w}d}{n:{w if n > 100 else 3}}|")
print(f"{n=:{w}}", f"{s=!r:{w}}")
