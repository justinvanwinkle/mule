r'''
schema_version = 1
name = "Task priority formatting"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-enumerate/tasks.py"
source_sha256 = "15844ae0e77892365e91983fb499007c20720bb4008cf2d2299bf89f1a54d1d5"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-enumerate/tasks.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "* PAY RENT!\n* Clean Dishes\n* Buy Milk\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

tasks_by_priority = ["Pay Rent", "Clean Dishes", "Buy Milk"]

for index, task in enumerate(tasks_by_priority):
    if index == 0:
        print(f"* {task.upper()}!")
    else:
        print(f"* {task}")
