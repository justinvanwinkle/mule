r'''
schema_version = 1
name = "Unicode string reversal"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/reverse-string/.meta/example.py"
source_sha256 = "3aeb3382cce0c00e099275fe3c6c91a5c0eb84ec3a7a06dbafadefe360e3f9ec"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/reverse-string/reverse_string_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "猫子\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "only fixed positional parameters are supported yet"
'''

def reverse(text=""):
    return text[::-1]


print(reverse("子猫"))
