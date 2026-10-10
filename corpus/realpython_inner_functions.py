r'''
schema_version = 1
name = "Nested functions and returning functions"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/primer-on-python-decorators/inner_functions.py"
source_sha256 = "0d3e901429d56ef865f5b67d35fe2d0812bb1866016a66979b7ffe440c58fec4"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/primer-on-python-decorators/inner_functions.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Printing from parent()\nPrinting from second_child()\nPrinting from first_child()\nHi, I'm Elias\nCall me Ester\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "nested functions are not supported yet"
'''

def parent():
    print("Printing from parent()")

    def first_child():
        print("Printing from first_child()")

    def second_child():
        print("Printing from second_child()")

    second_child()
    first_child()


def get_child(num):
    def first_child():
        return "Hi, I'm Elias"

    def second_child():
        return "Call me Ester"

    if num == 1:
        return first_child
    else:
        return second_child


if __name__ == "__main__":
    parent()

    first = get_child(1)
    second = get_child(2)
    print(first())
    print(second())
