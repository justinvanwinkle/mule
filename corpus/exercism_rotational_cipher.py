r'''
schema_version = 1
name = "Rotational cipher"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/rotational-cipher/.meta/example.py"
source_sha256 = "3719346c3cd1ccfc91f11d1590d5d332d51fa515830c44b901a411e36548ce48"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/rotational-cipher/rotational_cipher_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "TRL\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "only absolute compiler imports from mule are supported"
'''

from string import ascii_lowercase, ascii_uppercase


ALPHA_LEN = len(ascii_lowercase)


def rotate(message, key):
    coded_message = ''
    for char in message:
        if char in ascii_lowercase:
            char = ascii_lowercase[(ascii_lowercase.index(char) + key) % ALPHA_LEN]
        elif char in ascii_uppercase:
            char = ascii_uppercase[(ascii_uppercase.index(char) + key) % ALPHA_LEN]
        coded_message += char
    return coded_message


print(rotate("OMG", 5))
