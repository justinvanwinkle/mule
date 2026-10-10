r'''
schema_version = 1
name = "Extracting alternating text characters"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-enumerate/secret_message.py"
source_sha256 = "39c612d2c4aa9cb41ceb8ae2faeedb3b63f4818b8f51ce3f5c9a755109739280"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-enumerate/secret_message.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Life finds a way.\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "supported"
phase = "compile"
reason = ""
'''

secret_message = "3LAigf7eq 5fhiOnpdDs2 Ra6 nwUalyo.9"
message = ""

for index, char in enumerate(secret_message):
    if index % 2:
        message += char

print(message)
