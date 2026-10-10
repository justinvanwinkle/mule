r'''
schema_version = 1
name = "Hamming distance"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/hamming/.meta/example.py"
source_sha256 = "7daf615507fa7dade1f0e4989e5fbdc6d52dcd884f1851399fbdaa7a248ba84a"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/hamming/hamming_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "9\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "syntax is not supported by the native compiler yet"
'''

def distance(strand_a, strand_b):
    if len(strand_a) != len(strand_b):
        raise ValueError('Strands must be of equal length.')

    return sum(a_part != b_part for a_part, b_part in zip(strand_a, strand_b))


print(distance("GGACGGATTCTG", "AGGACGGATTCT"))
