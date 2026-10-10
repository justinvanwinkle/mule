r'''
schema_version = 1
name = "Inverting a number name dictionary"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/swap-loop.py"
source_sha256 = "ae048bae0eec3d237416ed9854a2ebf9581f0602698c341d64413ac03d06e320"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/swap-loop.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic print driver: print(swapped)."

[test]
stdout = "{1: 'one', 2: 'two', 3: 'three', 4: 'four'}\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

numbers = {"one": 1, "two": 2, "three": 3, "four": 4}
swapped = {}

for key, value in numbers.items():
    swapped[value] = key


print(swapped)
