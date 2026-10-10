r'''
schema_version = 1
name = "Function decorator before and after messages"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/primer-on-python-decorators/hello_decorator.py"
source_sha256 = "c6cb1d53d068e742a73d625b2a51d87229febb5857abf3a355082176f65f2e66"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/primer-on-python-decorators/hello_decorator.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Something is happening before the function is called.\nWhee!\nSomething is happening after the function is called.\nSomething is happening before the function is called.\nWhee!\nSomething is happening after the function is called.\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "nested functions are not supported yet"
'''

def decorator(func):
    def wrapper():
        print("Something is happening before the function is called.")
        func()
        print("Something is happening after the function is called.")

    return wrapper


def say_whee():
    print("Whee!")


say_whee = decorator(say_whee)


@decorator
def say_whee2():
    print("Whee!")


if __name__ == "__main__":
    say_whee()
    say_whee2()
