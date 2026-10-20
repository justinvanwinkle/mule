"""Translation table construction preserves Python's strings and validation."""

from mule.compiler import emit_llvm
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_numeric_call_conventions import numeric_entries


def test_exact_str_find_emits_mule_text_find_and_borrows_receiver():
    llvm = emit_llvm(
        """
def search(text: str, needle: str, pos: int) -> int:
    a = text.find(needle)
    b = text.find(needle, pos)
    c = text.find(needle, pos, pos + 10)
    d = text.rfind(needle, None, pos)
    return a + b + c + d

print(search("hello world", "o", 2))
"""
    )
    for symbol, _parameters, body in numeric_entries(llvm, "mule_fn_search", "i64"):
        assert f"call i64 {symbol}(" in llvm
        assert body.count("call ptr @mule_text_find(") == 4
        assert "@mule_resolve_method" not in body


def test_str_find_and_rfind_edge_cases(tmp_path):
    result = compare(
        tmp_path,
        """
def check(s: str, sub: str):
    results = [
        s.find(sub),
        s.rfind(sub),
        s.find(sub, 0),
        s.rfind(sub, 0),
        s.find(sub, None, None),
        s.rfind(sub, None, None),
    ]
    for start in (-10, -3, -1, 0, 1, 2, 5, 10):
        results.append(s.find(sub, start))
        results.append(s.rfind(sub, start))
        for end in (-10, -3, -1, 0, 1, 2, 5, 10):
            results.append(s.find(sub, start, end))
            results.append(s.rfind(sub, start, end))
    return results

for text in ("", "a", "aaaaa", "abcdeabcde", "héllo wörld héllo", "😀🐍😀🐍x"):
    for sub in ("", "a", "abc", "de", "ö", "hé", "🐍", "😀🐍", "missing", "😀🐍😀🐍xyz"):
        print(check(text, sub))
""",
    )
    assert result.stderr == ""


def test_maketrans_arbitrary_values_deletion_unicode_and_validation(tmp_path):
    result = compare(
        tmp_path,
        """
table = str.maketrans({'a': '', 'b': 'several', 'c': None, 100: 0x1f600})
print(table)
print('abcdxyz'.translate(table))
print(str.maketrans('abc', 'XYZ', 'a!'))
print('a!bc'.translate(str.maketrans('abc', 'XYZ', 'a!')))
print(str.maketrans({'a': 1.0}))
class Key(str):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, key): raise AssertionError('index')
print(str.maketrans({Key('a'): Key('replacement')}))
print(str.maketrans(Key('ab'), Key('XY'), Key('a')))
class Table(dict): pass
for args in ((), (1, 2, 3, 4), ([],), (Table(a='x'),),
             ({'ab': 'x'},), ({1.0: 'x'},), (b'a', 'b'),
             ('a', b'b'), ('a', 'b', None), ('ab', 'x')):
    try:
        str.maketrans(*args)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
for operation in (lambda: 'a'.find(None), lambda: 'a'.encode(None),
                  lambda: format('a', None)):
    try:
        operation()
    except TypeError as error:
        print(str(error))
""",
    )
    assert result.stderr == ""
