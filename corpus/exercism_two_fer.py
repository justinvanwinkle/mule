r'''
schema_version = 1
name = "Two-fer default argument"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/two-fer/.meta/example.py"
source_sha256 = "af93ba8264743e83d4d9b5b9bdab13ab40093b991f00a05695317cb86f2def0c"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/two-fer/two_fer_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "One for Alice, one for me.\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "supported"
phase = "compile"
reason = ""
'''

def two_fer(name='you'):
    return f'One for {name}, one for me.'


print(two_fer("Alice"))
