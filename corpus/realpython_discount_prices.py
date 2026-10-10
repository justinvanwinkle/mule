r'''
schema_version = 1
name = "Updating fruit prices"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/update-values.py"
source_sha256 = "85ef38b3d852cbb42230a423fa0b843a7ddef22fe9813b00988ed210b6dfceed"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/update-values.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic print driver: print(fruits)."

[test]
stdout = "{'apple': 0.36, 'orange': 0.32, 'banana': 0.23}\n"
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

for fruit, price in fruits.items():
    fruits[fruit] = round(price * 0.9, 2)


print(fruits)
