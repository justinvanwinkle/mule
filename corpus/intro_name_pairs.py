r'''
schema_version = 1
name = "Build unique pairs of names"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L3998"
source_sha256 = "244eeaa90547e8adc5957f290ed06e726b7d1f71370e181f0b24a2df59753315"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L3998"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Extracted tutorial console snippet, removed prompts and added explicit print calls for displayed expressions."

[test]
stdout = "[('Jo', 'Joe'), ('Jo', 'Jon'), ('Joe', 'Jon')]\n[('Jo', 'Joe'), ('Jo', 'Jon'), ('Joe', 'Jon')]\n"
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

names = ['Jo', 'Joe', 'Jon']
pairs = []
for i, n1 in enumerate(names):
    for n2 in names[i+1:]:
        pairs.append((n1, n2))
print(pairs)
print([(n1, n2) for i, n1 in enumerate(names) for n2 in names[i+1:]])
