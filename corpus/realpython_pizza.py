r'''
schema_version = 1
name = "Pizza toppings and class methods"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/instance-class-static-methods/pizza.py"
source_sha256 = "39edbe8b7f840d581f63287d271278b1e6a8591a8bc7ec0829308d09808ac179"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/instance-class-static-methods/pizza.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Pizza(['mozzarella', 'tomatoes']) 😋🍕\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

class Pizza:
    def __init__(self, toppings):
        self.toppings = list(toppings)

    def __repr__(self):
        return f"Pizza({self.toppings})"

    def add_topping(self, topping):
        self.toppings.append(topping)

    def remove_topping(self, topping):
        if topping in self.toppings:
            self.toppings.remove(topping)

    @classmethod
    def margherita(cls):
        return cls(["mozzarella", "tomatoes"])

    @classmethod
    def prosciutto(cls):
        return cls(["mozzarella", "tomatoes", "ham"])

    @staticmethod
    def get_size_in_inches(size):
        """Returns the diameter in inches for common pizza sizes."""
        size_map = {
            "small": 8,
            "medium": 12,
            "large": 16,
        }
        return size_map.get(size, "Unknown size")


if __name__ == "__main__":
    a_pizza = Pizza.margherita()
    print(a_pizza, "😋🍕")
