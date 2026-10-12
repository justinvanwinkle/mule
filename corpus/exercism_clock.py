r'''
schema_version = 1
name = "Clock normalization"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/clock/.meta/example.py"
source_sha256 = "91f83a5bfbdda1a953599b8c04d699a310a4ec32a6a65bb42649237e452cb6aa"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/clock/clock_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "03:40\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "class bodies support only instance methods, bare typed native fields, and pass"
'''

class Clock:
    """Clock that displays 24 hour clock that rollsover properly"""

    def __init__(self, hour, minute):
        self.hour = hour
        self.minute = minute
        self.cleanup()

    def __repr__(self):
        return f'Clock({self.hour}, {self.minute})'

    def __str__(self):
        return f'{self.hour:02d}:{self.minute:02d}'

    def __eq__(self, other):
        return repr(self) == repr(other)

    def __add__(self, minutes):
        self.minute += minutes
        return self.cleanup()

    def __sub__(self, minutes):
        self.minute -= minutes
        return self.cleanup()

    def cleanup(self):
        self.hour += self.minute // 60
        self.hour %= 24
        self.minute %= 60
        return self


print(str(Clock(25, 160)))
