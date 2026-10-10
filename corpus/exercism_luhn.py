r'''
schema_version = 1
name = "Luhn checksum"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/luhn/.meta/example.py"
source_sha256 = "8c9b705167891902cc94605dd71668082cc7b8cd2b67cc1054e40755d38b5d23"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/luhn/luhn_test.py"
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
phase = "run"
reason = "AttributeError: object has no attribute 'replace'"

[mule.failure]
stdout = ""
stderr = "AttributeError: object has no attribute 'replace'\n"
exit_code = 1
'''

class Luhn:
    def __init__(self, card_num):
        self.card_num = card_num
        self.checksum = -1
        digits = card_num.replace(' ', '')
        length = len(digits)
        if digits.isdigit() and length > 1:
            self.checksum = 0
            cadence = length % 2
            for idx, digit in enumerate(digits):
                num = int(digit)
                if idx % 2 == cadence:
                    num *= 2
                    if num > 9:
                        num -= 9
                self.checksum += num

    def valid(self):
        return self.checksum % 10 == 0


print(Luhn("055 444 285").valid())
