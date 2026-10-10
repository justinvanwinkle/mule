r'''
schema_version = 1
name = "Currency object fields"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-property/currency_v1.py"
source_sha256 = "3916325204023b7f9e38fd2c20f16ddd81147872a0dcb2201f74945e924ea7ef"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-property/currency_v1.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic print driver: currency = Currency(10, 25); print(currency.units, currency.cents)."

[test]
stdout = "10 25\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "supported"
phase = "compile"
reason = ""
'''

class Currency:
    def __init__(self, units, cents):
        self.units = units
        self.cents = cents

    # Currency implementation...


currency = Currency(10, 25)
print(currency.units, currency.cents)
