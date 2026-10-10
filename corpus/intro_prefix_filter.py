r'''
schema_version = 1
name = "Filter words by multiple prefixes"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L3803"
source_sha256 = "244eeaa90547e8adc5957f290ed06e726b7d1f71370e181f0b24a2df59753315"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L3803"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Extracted tutorial console snippet, removed prompts and added explicit print calls for displayed expressions."

[test]
stdout = "refuse\nimpossible\nread\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0
args = []

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

words = ['refuse', 'impossible', 'present', 'read']
prefix = ('im', 're')
for w in words:
    if w.startswith(prefix):
        print(w)
