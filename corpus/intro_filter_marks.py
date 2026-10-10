r'''
schema_version = 1
name = "Filter a student marks dictionary"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L4015"
source_sha256 = "244eeaa90547e8adc5957f290ed06e726b7d1f71370e181f0b24a2df59753315"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L4015"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Extracted tutorial console snippet, removed prompts and added explicit print calls for displayed expressions."

[test]
stdout = "{'Ravi': 92, 'Rajan': 85, 'Ram': 80}\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0
args = []

[mule]
status = "pending"
phase = "compile"
reason = "only positional call arguments are supported yet"
'''

marks = dict(Rahul=68, Ravi=92, Rohit=75, Rajan=85, Ram=80)
print({k: v for k, v in marks.items() if v >= 80})
