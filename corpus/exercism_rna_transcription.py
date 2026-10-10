r'''
schema_version = 1
name = "RNA transcription"
source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/rna-transcription/.meta/example.py"
source_sha256 = "7ddeb2f48bb19ddb75218b7cd0af28d416934bd870839ba316db6319fd9ab42e"
expectation_source = "https://github.com/exercism/python/blob/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706/exercises/practice/rna-transcription/rna_transcription_test.py"
license = "MIT"
license_file = "licenses/Exercism-MIT.txt"
adaptation = "Prepended TOML module docstring and appended deterministic print driver; upstream implementation unchanged."

[test]
stdout = "UGCACCAGAAUU\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "run"
reason = "AttributeError: object has no attribute 'maketrans'"

[mule.failure]
stdout = ""
stderr = "AttributeError: object has no attribute 'maketrans'\n"
exit_code = 1
'''

DNA_TO_RNA = str.maketrans("AGCT", "UCGA")

def to_rna(dna_strand):
    return dna_strand.translate(DNA_TO_RNA)


print(to_rna("ACGTGGTCTTAA"))
