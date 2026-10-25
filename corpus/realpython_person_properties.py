r'''
schema_version = 1
name = "Person property setters and inherited getters"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-property/persons.py"
source_sha256 = "657a93717584ad43061ac563bfd045d42b2c5fdbad96c589b49d2c1c7b312135"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-property/persons.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic print driver: person = Person('Alice'); print(person.name); person.name = 'Bob'; print(person.name); employee = Employee('Carol'); print(employee.name)."

[test]
stdout = "Alice\nBob\nCAROL\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "only imported mule_types.optimize decorators are supported"
'''

class Person:
    def __init__(self, name):
        self._name = name

    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, value):
        self._name = value

    # Person implementation...


class Employee(Person):
    @property
    def name(self):
        return super().name.upper()

    # Employee implementation...


person = Person('Alice')
print(person.name)
person.name = 'Bob'
print(person.name)
employee = Employee('Carol')
print(employee.name)
