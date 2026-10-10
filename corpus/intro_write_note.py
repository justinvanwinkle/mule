r'''
schema_version = 1
name = "Write and read a text note"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/dealing_with_files/write_file.py"
source_sha256 = "9d8ae6f81ac4cd88f3a2f8587fa48f6a39ccaac922acb5ea05cf2e5b13567b62"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/dealing_with_files/write_file.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic output driver."

[test]
stdout = "this is a sample line of text\nyet another line\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0
args = []

[mule]
status = "pending"
phase = "run"
reason = "NameError: name 'open' is not defined"

[mule.failure]
stdout = ""
stderr = "NameError: name 'open' is not defined\n"
exit_code = 1
'''

with open('op.txt', mode='w', encoding='ascii') as f:
    f.write('this is a sample line of text\n')
    f.write('yet another line\n')


with open('op.txt', encoding='ascii') as f:
    print(f.read(), end='')
