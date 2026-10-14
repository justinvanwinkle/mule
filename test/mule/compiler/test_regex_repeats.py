"""Regex repetition preserves backtracking order without input-sized recursion."""

import pytest

from test.mule.compiler.test_exception_groups import compare


@pytest.mark.integration
def test_regex_repeat_order_captures_and_long_json_strings(tmp_path):
    native = compare(
        tmp_path,
        r"""
import re
import json
for pattern in ('a*', 'a*?', '(a|aa)+', '(aa|a)+', '(a?)*', '(a?)*?',
                '(){2}', '(?:){2}', '(a*)*', '(a|ab)+b', '(a|ab)+?b',
                '(a|ab)++b', 'a*+a', 'a*a', '(?:a|aa){1,3}a'):
    for text in ('', 'a', 'aa', 'aaa', 'ab', 'aab'):
        match = re.match(pattern, text)
        print(pattern, text, None if match is None else (match.span(), match.groups()))
text = 'x' * 5000 + 'end'
for pattern in ('x+', 'x+?', 'x*end', 'x*?end', '(x)+end', 'x++end'):
    match = re.match(pattern, text)
    print(pattern, match.span(), match.groups())
for pattern in ('[a-z]+end', '[^x]{1,4}x', '.{1,3}?a', r'\w{1,3}a',
                '(?i:a+Aa)', '(?i:[a-z]+end)', '[a-z]++end', r'\d*?x'):
    for text in ('', 'abcend', 'AAAa', '123x', '\nabc', 'x', 'aax'):
        match = re.match(pattern, text)
        print(pattern, text, None if match is None else (match.span(), match.groups()))
for pattern in (b'a*?a', b'[^x]+x', b'(?i:a+)a', br'\d{1,3}x'):
    for text in (b'', b'aa', b'AAa', b'123x'):
        match = re.match(pattern, text)
        print(pattern, text, None if match is None else (match.span(), match.groups()))
document = '{"key":"' + 'x' * 5000 + '","escaped":"a\\\\b","number":42}'
value = json.loads(document)
print(len(value['key']), value['escaped'], value['number'])
""",
    )
    assert native.stderr == ""
