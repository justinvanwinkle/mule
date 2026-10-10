r'''
schema_version = 1
name = "Customizable greeting styles"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/functions/default_args.py"
source_sha256 = "71b8cdde9c2701e3e575352ecacf30bbd67d1401d532b757e7943fb8f5ff639a"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/functions/default_args.py"
expectation_origin = "Deterministic input checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "------------\n     hi     \n------------\n--------\n  bye   \n--------\n===============\n     hello     \n===============\n::::::::::\n good day \n::::::::::\n"
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

def greeting(ip, style='-', spacing=10):
    op_length = spacing + len(ip)
    styled_line = style * op_length
    print(styled_line)
    print(f'{ip:^{op_length}}')
    print(styled_line)

greeting('hi')
greeting('bye', spacing=5)
greeting('hello', style='=')
greeting('good day', ':', 2)

