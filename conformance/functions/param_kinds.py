"""Parameter kinds: positional-only, keyword-only, defaults, *args, **kwargs, and all combined."""


def pos_only(a, b, /):
    return (a, b)


def kw_only(*, a, b=2):
    return (a, b)


def mixed(a, b=10, /, c=20, *, d, e=30):
    return (a, b, c, d, e)


def star_args(*args):
    return args


def star_kwargs(**kwargs):
    return sorted(kwargs.items())


def everything(a, b=1, /, c=2, *args, d, e=4, **kwargs):
    return (a, b, c, args, d, e, sorted(kwargs.items()))


def pos_only_name_in_kwargs(a, /, **kwargs):
    # 'a' passed by keyword lands in kwargs because a is positional-only
    return (a, sorted(kwargs.items()))


def args_then_kwonly(*args, sep="-"):
    return sep.join(str(x) for x in args)


def default_then_star(a=1, *args):
    return (a, args)


print("pos_only", pos_only(1, 2))
print("kw_only default", kw_only(a=1))
print("kw_only both", kw_only(b=5, a=6))
print("mixed minimal", mixed(1, d=4))
print("mixed full", mixed(1, 2, 3, d=4, e=5))
print("mixed c by kw", mixed(1, c=99, d=4))
print("star_args empty", star_args())
print("star_args many", star_args(1, "x", None))
print("star_args type", type(star_args(1)).__name__)
print("star_kwargs", star_kwargs(z=1, a=2, m=3))
print("star_kwargs type", type((lambda **k: k)()).__name__)
print("everything min", everything(0, d=9))
print("everything max", everything(0, 1, 2, 3, 4, 5, d=6, e=7, f=8, g=9))
print("pos_only name in kwargs", pos_only_name_in_kwargs(1, a=2, b=3))
print("args_then_kwonly", args_then_kwonly(1, 2, 3))
print("args_then_kwonly sep", args_then_kwonly(1, 2, 3, sep="+"))
print("default_then_star none", default_then_star())
print("default_then_star some", default_then_star(5, 6, 7))


def bare_star_only(*, x):
    return x * 2


print("bare_star_only", bare_star_only(x=21))


def kwargs_preserves_order(**kw):
    return list(kw)


print("kwargs order", kwargs_preserves_order(c=1, a=2, b=3))


def kwargs_is_fresh(**kw):
    kw["added"] = True
    return kw


source = {"x": 1}
result = kwargs_is_fresh(**source)
print("kwargs fresh dict", sorted(result.items()), sorted(source.items()))


def args_is_tuple_even_from_list(*args):
    return type(args).__name__, args


print("args from list", args_is_tuple_even_from_list(*[1, 2]))


def weird_names(self, cls, /, *args, **kwargs):
    return (self, cls, args, sorted(kwargs.items()))


print("weird names", weird_names(1, 2, 3, self=4, cls=5))


def kwonly_no_default_after_default(a=1, *, b, c=3, d):
    return (a, b, c, d)


print("kwonly order", kwonly_no_default_after_default(b=2, d=4))
