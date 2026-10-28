"""Global and nested containers lose stale size facts after mutation."""

from test.mule.compiler.test_native import run_native


def test_global_list_mutation_updates_length_and_iteration(tmp_path):
    result = run_native(
        tmp_path,
        """
items = []
for value in range(3):
    items.append(value)
print(items, len(items), [value for value in items])
items = ['a', 'b']
seen = []
for value in items:
    seen.append(value)
    if len(items) < 4:
        items.insert(0, 'new')
print(seen, items)
table = [[lambda: (row, column) for column in range(2)] for row in range(2)]
print([[function() for function in row] for row in table])
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "[0, 1, 2] 3 [0, 1, 2]\n"
        "['a', 'a', 'a', 'b'] ['new', 'new', 'a', 'b']\n"
        "[[(1, 1), (1, 1)], [(1, 1), (1, 1)]]\n"
    )


def test_dynamic_globals_shadowing_preserves_builtin_lookup(tmp_path):
    result = run_native(
        tmp_path,
        """
globals()['max'] = lambda *values: 'shadowed'
print(max(1, 2))
del globals()['max']
print(max(1, 2))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "shadowed\n2\n"
