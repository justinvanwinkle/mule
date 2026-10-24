r'''
schema_version = 1
name = "Formatted product price property"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-property/product.py"
source_sha256 = "ef943ba8c5485ec03384a29a87f34910f2504f548d743927e1754185e23b2955"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-property/product.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged. Appended deterministic print driver: product = Product('Python Book', 12.5); print(product.price)."

[test]
stdout = "$12.50\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "only imported mule_types.optimize decorators are supported"
'''

class Product:
    def __init__(self, name, price):
        self._name = name
        self._price = float(price)

    @property
    def price(self):
        return f"${self._price:,.2f}"


product = Product('Python Book', 12.5)
print(product.price)
