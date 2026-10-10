r'''
schema_version = 1
name = "Read and capitalize file lines"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/dealing_with_files/read_file.py"
source_sha256 = "2aa6aa248dc11c00d5c2db917557a4984991b260e34230b3f80b9b3b85596276"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/dealing_with_files/read_file.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Hello world.\nThis is a sample.\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0
args = []

[files]
"ip.txt" = "hello world\nthis is a sample\n"

[mule]
status = "pending"
phase = "run"
reason = "NameError: name 'open' is not defined"

[mule.failure]
stdout = ""
stderr = "NameError: name 'open' is not defined\n"
exit_code = 1
'''

with open('ip.txt', mode='r', encoding='ascii') as f:
    for ip_line in f:
        op_line = ip_line.rstrip('\n').capitalize() + '.'
        print(op_line)

