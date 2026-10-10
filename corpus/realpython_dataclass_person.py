r'''
schema_version = 1
name = "Iterating person dataclass fields"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-namedtuple/person_dataclass.py"
source_sha256 = "8daec6a59c24cadac3dead3b7b7e4e727c8965dc4ad368ad9e01311a89466be8"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-namedtuple/person_dataclass.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Jane\n25\n1.75\n67\nCanada\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "only absolute compiler imports from mule are supported"
'''

from dataclasses import astuple, dataclass


@dataclass
class Person:
    name: str
    age: int
    height: float
    weight: float
    country: str = "Canada"

    def __iter__(self):
        return iter(astuple(self))


jane = Person("Jane", 25, 1.75, 67)
for field in jane:
    print(field)
