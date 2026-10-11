r'''
schema_version = 1
name = "Command-line file extension sorter"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/cli_args/sort_ext_stdin.py"
source_sha256 = "74a2d04b3dbb10a4bbd9d47f2f40253140dafe1dd4266a5a5cac6cbb987a623d"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/programs/cli_args/sort_ext_stdin.py"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "main.py\nREADME.txt\nnotes.txt\narchive.zip\n"
stderr = ""
exit_code = 0
stdin = "notes.txt\narchive.zip\nmain.py\nREADME.txt\nmain.py\n"
timeout = 5.0
args = ["--unique"]

[mule]
status = "pending"
phase = "compile"
reason = "class bodies support only a leading docstring, instance methods, bare typed native fields, and pass"
compile_error_file = "stdlib/abc.py"
compile_error_line = 41
'''

import argparse, sys

parser = argparse.ArgumentParser()
parser.add_argument('file', nargs='?',
                    type=argparse.FileType('r'), default=sys.stdin,
                    help="input file to be sorted")
parser.add_argument('-u', '--unique', action='store_true',
                    help="sort uniquely")
args = parser.parse_args()

ip_lines = args.file.readlines()
if args.unique:
    ip_lines = set(ip_lines)

op_lines = sorted(ip_lines, key=lambda s: (s.rsplit('.', 1)[-1], s))
for line in op_lines:
    print(line, end='')
