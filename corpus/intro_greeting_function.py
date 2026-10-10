r'''
schema_version = 1
name = "Function prints a greeting"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/functions/no_args.py"
source_sha256 = "6ee5f768c9f9d9f1f07ad22fab5316ad6b0ef3356a3ef5d436f6083541082ea6"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/functions/no_args.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "-----------------------------\n         Hello World         \n-----------------------------\n"
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

def greeting():
    print('-----------------------------')
    print('         Hello World         ')
    print('-----------------------------')

greeting()
