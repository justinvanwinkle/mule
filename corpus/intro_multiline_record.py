r'''
schema_version = 1
name = "Multiline student record"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/strings_user_input/triple_quotes.py"
source_sha256 = "4f617d7b2b88c9ef27faf8343fd7f41fea14ed3550183825c260ce33ac5f2d4a"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/strings_user_input/triple_quotes.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "hi there.\nhow are you?\nName:\tlearnbyexample\nAge:\t25\nDept:\tCSE\n"
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

print('''hi there.
how are you?''')

student = '''\
Name:\tlearnbyexample
Age:\t25
Dept:\tCSE'''

print(student)
