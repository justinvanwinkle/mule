r'''
schema_version = 1
name = "Leap year"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/leap/.meta/example.py"
source_sha256 = "0284bd1228151f679b12ad7c481f5deb470e7bea1a795a546dae04d47aff8cd3"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/leap/leap_test.py"
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
status = "supported"
phase = "compile"
reason = ""
'''

def leap_year(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)


print(leap_year(2000))
