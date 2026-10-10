r'''
schema_version = 1
name = "Passing greeting functions"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/primer-on-python-decorators/greeters.py"
source_sha256 = "8bfbe771031ce4b96fac537f443505b21aa17131b302f55948dfd2b8e5ea82f8"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/primer-on-python-decorators/greeters.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Hello Bob\nYo Bob, together we're the awesomest!\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

def say_hello(name):
    return f"Hello {name}"


def be_awesome(name):
    return f"Yo {name}, together we're the awesomest!"


def greet_bob(greeter_func):
    return greeter_func("Bob")


if __name__ == "__main__":
    print(greet_bob(say_hello))
    print(greet_bob(be_awesome))
