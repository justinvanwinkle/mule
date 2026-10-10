r'''
schema_version = 1
name = "Generator filters dish names"
source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L2892"
source_sha256 = "244eeaa90547e8adc5957f290ed06e726b7d1f71370e181f0b24a2df59753315"
expectation_source = "https://github.com/learnbyexample/100_page_python_intro/blob/47f92dfae336a650849e5598deeb9f37e2d05996/100_page_python_intro.md#L2892"
expectation_origin = "Published example checked with isolated CPython"
license = "MIT"
license_file = "licenses/PythonIntro-MIT.txt"
adaptation = "Extracted tutorial console snippet, removed prompts and added explicit print calls for displayed expressions."

[test]
stdout = "('Poha', 'Baati')\n"
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

dishes = ('Poha', 'Aloo tikki', 'Baati', 'Khichdi', 'Makki roti')
print(tuple(d for d in dishes if len(d) < 6))
