r'''
schema_version = 1
name = "Interactive countdown"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/control_structures/countdown.py"
source_sha256 = "a329a486c19793c1ba28c2adbae0d25444d5af8c6979a59a527742f8aa4e42bc"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/control_structures/countdown.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Enter a positive integer: 3\n2\n1\nGo!\n"
stderr = ""
exit_code = 0
stdin = "3\n"
timeout = 5.0
args = []

[mule]
status = "supported"
phase = "compile"
reason = ""
'''

count = int(input('Enter a positive integer: '))
while count > 0:
    print(count)
    count -= 1

print('Go!')
