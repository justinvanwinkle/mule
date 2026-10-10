r'''
schema_version = 1
name = "Removing expensive fruit entries"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/remove-key.py"
source_sha256 = "6ef7200073cf350e44faed38e4881f8c28aa7207a8b7a7a4630fbbd0db6e5bda"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/remove-key.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic print driver: print(fruits)."

[test]
stdout = "{'banana': 0.25}\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

fruits = {"apple": 0.40, "orange": 0.35, "banana": 0.25}

for fruit in fruits.copy():
    if fruits[fruit] >= 0.30:
        del fruits[fruit]


print(fruits)
