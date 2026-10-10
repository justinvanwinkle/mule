r'''
schema_version = 1
name = "Isogram detection"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/isogram/.meta/example.py"
source_sha256 = "5867393671ccf45c615716034faa5ed981a7c0bf29e89cc1d5a422aaa759e9d9"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/isogram/isogram_test.py"
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
reason = "syntax is not supported by the native compiler yet"
'''

def is_isogram(string):
    characters_lower = [char.lower() for char in string if char.isalpha()]
    return len(set(characters_lower)) == len(characters_lower)


print(is_isogram("subdermatoglyphic"))
