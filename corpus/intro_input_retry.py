r'''
schema_version = 1
name = "Recover from invalid interactive input"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/exceptions/try_except_else.py"
source_sha256 = "ed90b14b0106be1694267d45915251eff9cc438040061f13ea92818cbd734a51"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/exceptions/try_except_else.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Enter an integer number: Not an integer, try again\nEnter an integer number: Square of 3 is 9\n"
stderr = ""
exit_code = 0
stdin = "bad\n3\n"
timeout = 5.0
args = []

[mule]
status = "supported"
phase = "compile"
reason = ""
'''

while True:
    try:
        num = int(input('Enter an integer number: '))
    except ValueError:
        print('Not an integer, try again')
    else:
        print(f'Square of {num} is {num ** 2}')
        break

