r'''
schema_version = 1
name = "Variable-width message banners"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/functions/with_args.py"
source_sha256 = "6618f8e790b262c104136957558f17bf1e64b3aba443692209354e3b83afb24e"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/functions/with_args.py"
expectation_origin = "Deterministic input checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "------------\n     hi     \n------------\n------------------------------------------\n     Today would be a nice, sunny day     \n------------------------------------------\n"
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

def greeting(ip):
    op_length = 10 + len(ip)
    styled_line = '-' * op_length
    print(styled_line)
    print(f'{ip:^{op_length}}')
    print(styled_line)

greeting('hi')
weather = 'Today would be a nice, sunny day'
greeting(weather)

