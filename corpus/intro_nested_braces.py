r'''
schema_version = 1
name = "Validate nested braces in expressions"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/testing/nested_braces.py"
source_sha256 = "4a84f3e114672ea228f282cc116d89a93a841c08c57fab8e91d8f1128a3386a9"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/testing/nested_braces.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "all tests passed\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0
args = []

[mule]
status = "supported"
phase = "compile"
reason = ""
'''

def max_nested_braces(expr):
    max_count = count = 0
    for char in expr:
        if char == '{':
            count += 1
            if count > max_count:
                max_count = count
        elif char == '}':
            if count == 0:
                return -1
            count -= 1

    if count != 0:
        return -1
    return max_count

def test_cases():
    assert max_nested_braces('a*b') == 0
    assert max_nested_braces('a*b+{}') == 1
    assert max_nested_braces('a*{b+c}') == 1
    assert max_nested_braces('{a+2}*{b+c}') == 1
    assert max_nested_braces('a*{b+c*{e*3.14}}') == 2
    assert max_nested_braces('{{a+2}*{b+c}+e}') == 2
    assert max_nested_braces('{{a+2}*{b+{c*d}}+e}') == 3
    assert max_nested_braces('{{a+2}*{{b+{c*d}}+e*d}}') == 4
    assert max_nested_braces('a*b{') == -1
    assert max_nested_braces('a*{b+c}}') == -1
    assert max_nested_braces('}a+b{') == -1
    assert max_nested_braces('a*{b+c*{e*3.14}}}') == -1
    assert max_nested_braces('{{a+2}*{{b}+{c*d}}+e*d}}') == -1

if __name__ == '__main__':
    test_cases()
    print('all tests passed')

