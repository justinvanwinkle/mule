r'''
schema_version = 1
name = "Allergy bitmask"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/allergies/.meta/example.py"
source_sha256 = "718ff6d0c828237aea54204d01c23e57da23908a4a0558d328a01a6e707ad417"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/allergies/allergies_test.py"
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
reason = "class bodies support only instance methods, bare typed native fields, and pass"
'''

class Allergies:

    _allergies = [
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats"
    ]

    def __init__(self, score):
        self.score = score

    def allergic_to(self, item):
        return bool(self.score & 1 << self._allergies.index(item))

    @property
    def lst(self):
        return [allergy for allergy in self._allergies
                if self.allergic_to(allergy)]


print(Allergies(255).allergic_to("eggs"))
