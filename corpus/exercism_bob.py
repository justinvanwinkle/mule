r'''
schema_version = 1
name = "Bob response"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/bob/.meta/example.py"
source_sha256 = "f9162a6932c526d8ce23a710ddd488fb3c8e02115ba3bd9702a8da316b19242b"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/bob/bob_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "Whoa, chill out!\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "run"
reason = "AttributeError: object has no attribute 'strip'"

[mule.failure]
stdout = ""
stderr = "AttributeError: object has no attribute 'strip'\n"
exit_code = 1
'''

def response(hey_bob):
    hey_bob = hey_bob.strip()

    if _is_silence(hey_bob):
        return 'Fine. Be that way!'
    if _is_shouting(hey_bob):
        if _is_question(hey_bob):
            return "Calm down, I know what I'm doing!"
        else:
            return 'Whoa, chill out!'
    elif _is_question(hey_bob):
        return 'Sure.'
    else:
        return 'Whatever.'


def _is_silence(hey_bob):
    return hey_bob == ''


def _is_shouting(hey_bob):
    return hey_bob.isupper()


def _is_question(hey_bob):
    return hey_bob.endswith('?')


print(response("WATCH OUT!"))
