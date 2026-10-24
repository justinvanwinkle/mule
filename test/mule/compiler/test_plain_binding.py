"""Calls without keywords bind defaults and keyword-only defaults in the plain
external entry, without the general binder; wrong arities still raise."""

from test.mule.compiler.test_exception_groups import compare


def test_positional_calls_fill_defaults_and_keyword_only_defaults(tmp_path):
    compare(
        tmp_path,
        """
def plain(a, b=2, c=3):
    return (a, b, c)

def rest(a, *args, key=None, flag=True):
    return (a, args, key, flag)

def mixed(a, b=[], /, c=None, *, d=4):
    return (a, b, c, d)

def kwargs(a, b=1, **extra):
    return (a, b, extra)

def rest_default(a, b=2, *args, k=3):
    return (a, b, args, k)

calls = [plain, rest, mixed, kwargs, rest_default]
arguments = [(), (1,), (1, 2), (1, 2, 3), (1, 2, 3, 4), (1, "x", None, 4, 5)]
for function in calls:
    for args in arguments:
        try:
            print(function.__name__, function(*args))
        except TypeError:
            print(function.__name__, len(args), "TypeError")
print(plain(1, c=5), rest(1, 2, key="k"), mixed(1, d=0), kwargs(1, z=2))
shared = plain(1)
print(shared, plain(0)[1] is shared[1])
print(rest(1, 2, 3)[1] == (2, 3), mixed(1)[1] is mixed(2)[1])
""",
    )


def test_plain_entry_binds_omitted_defaults_without_the_general_binder():
    from mule.compiler import emit_llvm

    llvm = emit_llvm(
        "def f(a, b=2, *, k=3):\n    return a + b + k\ng = f\nprint(g(1))\n"
    )
    entries = llvm.split("\ndefine ")[1:]
    wrapper = next(
        entry for entry in entries if entry.startswith('ptr @"mule_fn_f.fn"(')
    )
    assert (
        "call i32 @mule_binding_positional(ptr %args, i32 1, i32 2, i32 0)" in wrapper
    )
    assert "call ptr @mule_arg_or_default(ptr %args, i32 1, i32 0)" in wrapper
    assert "call ptr @mule_binding_default_at(ptr %args, i32 1)" in wrapper


def test_unused_variadic_parameters_skip_tuple_and_dict_allocation(tmp_path):
    from mule.compiler import emit_llvm

    source = """
def unused_both(a, *args, **kwargs):
    return a + 1

def used_args(a, *args, **kwargs):
    return (a, args)

g1 = unused_both
g2 = used_args
print(g1(10), g1(10, 20, 30, x=1, y=2))
print(g2(10, 20, x=1))
"""
    compare(tmp_path, source)
    llvm = emit_llvm(source)
    entries = {entry.split("(", 1)[0]: entry for entry in llvm.split("\ndefine ")[1:]}
    for name in (
        'ptr @"mule_fn_unused_both.fn"',
        'internal ptr @"mule_fn_unused_both.fn.binding"',
        'internal ptr @"mule_fn_unused_both.fn.view"',
    ):
        wrapper = entries[name]
        assert "@mule_rest_args" not in wrapper
        assert "build_dict" not in wrapper
    assert "@mule_rest_args" in entries['ptr @"mule_fn_used_args.fn"']
    assert "build_dict" not in entries['ptr @"mule_fn_used_args.fn"']
