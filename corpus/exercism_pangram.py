r'''
schema_version = 1
name = "Pangram detection"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/pangram/.meta/example.py"
source_sha256 = "c0b3fdd0c2783d6864e7c86782df3dd79117ebbf474d0b720f5a23a89dca087c"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/pangram/pangram_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "True\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "only absolute compiler imports from mule are supported"
'''

from string import ascii_lowercase


def is_pangram(sentence):
    return all(char in sentence.lower() for char in ascii_lowercase)


print(is_pangram("the quick brown fox jumps over the lazy dog"))
