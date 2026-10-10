r'''
schema_version = 1
name = "Input validation and final cleanup message"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/exceptions/try_except_finally.py"
source_sha256 = "1c38c2ee2670fb941cff6a04ad48e8b3dcd4fbd245068f55e790ce56e19b3df6"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/exceptions/try_except_finally.py"
expectation_origin = "Deterministic input checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Enter a positive integer: Not a positive integer, run the program again\n\nThanks for using the program, have a nice day\n"
stderr = ""
exit_code = 0
stdin = "-1\n"
timeout = 5.0
args = []

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

try:
    num = int(input('Enter a positive integer: '))
    if num < 0:
        raise ValueError
except ValueError:
    print('Not a positive integer, run the program again')
else:
    print(f'Square root of {num} is {num ** 0.5:.3f}')
finally:
    print('\nThanks for using the program, have a nice day')

