r'''
schema_version = 1
name = "Summing fruit sales income"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/total-income.py"
source_sha256 = "03e393c40ab82c209a953d8333b02182a06f54e52707ed97c3bd6f173ff27688"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/iterate-through-dictionary-python/total-income.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic print driver: print(total_income)."

[test]
stdout = "14100.0\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

incomes = {"apple": 5600.00, "orange": 3500.00, "banana": 5000.00}
total_income = 0.00

for income in incomes.values():
    total_income += income


print(total_income)
