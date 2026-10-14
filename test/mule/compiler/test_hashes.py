"""Streaming digest values compared with Python's implementation."""

from test.mule.compiler.test_exception_groups import compare


def test_blake2_keyed_parameters_last_blocks_and_copies(tmp_path):
    result = compare(
        tmp_path,
        """
from _blake2 import blake2b, blake2s
for factory in (blake2b, blake2s):
    block = factory().block_size
    for options in ({}, {'key': b'key'}, {'digest_size': 13},
                    {'salt': b'salt', 'person': b'context'},
                    {'fanout': 2, 'depth': 2, 'leaf_size': 100,
                     'node_offset': 3, 'node_depth': 1, 'inner_size': 16, 'last_node': True}):
        for data in (b'', b'abc', bytes(range(256)), b'x' * (block - 1),
                     b'x' * block, b'x' * (block + 1), b'x' * (2 * block)):
            value = factory(data, **options)
            print(value.name, value.block_size, value.digest_size, value.hexdigest())
            incremental = factory(**options)
            for offset in range(0, len(data), 7):
                incremental.update(memoryview(data[offset:offset + 7]))
            print(incremental.digest() == value.digest())
        copied = value.copy()
        value.update(b'left')
        copied.update(bytearray(b'right'))
        print(value.hexdigest(), copied.hexdigest())
        print(value.digest() == value.digest())
""",
    )
    assert result.stderr == ""


def test_sha3_domains_rate_boundaries_and_shake_squeezing(tmp_path):
    result = compare(
        tmp_path,
        """
from _sha3 import sha3_224, sha3_256, sha3_384, sha3_512, shake_128, shake_256
for factory in (sha3_224, sha3_256, sha3_384, sha3_512):
    rate = factory().block_size
    for data in (b'', b'abc', bytes(range(256)), b'x' * (rate - 1),
                 b'x' * rate, b'x' * (rate + 1)):
        value = factory(data)
        print(value.name, value.block_size, value.digest_size, value.hexdigest())
        incremental = factory()
        for offset in range(0, len(data), 13):
            incremental.update(memoryview(data[offset:offset + 13]))
        print(incremental.digest() == value.digest())
    original = factory(b'prefix')
    copied = original.copy()
    original.update(b'left')
    copied.update(bytearray(b'right'))
    print(original.hexdigest(), copied.hexdigest())
for factory in (shake_128, shake_256):
    rate = factory().block_size
    for data in (b'', b'abc', b'x' * (rate - 1), b'x' * rate, b'x' * (rate + 1)):
        value = factory(data, usedforsecurity=False)
        print(value.name, value.block_size, value.digest_size)
        for length in (0, 1, rate, rate + 1, 2 * rate + 1):
            print(value.hexdigest(length))
        copied = value.copy()
        copied.update(b'extra')
        print(copied.hexdigest(32), value.hexdigest(32))
    try:
        factory().digest(-1)
    except ValueError as error:
        print(str(error))
""",
    )
    assert result.stderr == ""


def test_sha_streaming_padding_copies_and_word_widths(tmp_path):
    result = compare(
        tmp_path,
        """
from _sha1 import sha1
from _sha2 import sha224, sha256, sha384, sha512
for factory in (sha1, sha224, sha256, sha384, sha512):
    for data in (b'', b'abc', bytes(range(256)), b'x' * 55, b'x' * 56,
                 b'x' * 63, b'x' * 64, b'x' * 111, b'x' * 112,
                 b'x' * 127, b'x' * 128, b'x' * 129):
        value = factory(data, usedforsecurity=False)
        print(value.name, value.block_size, value.digest_size, value.hexdigest())
        incremental = factory()
        for offset in range(0, len(data), 7):
            incremental.update(memoryview(data[offset:offset + 7]))
        print(incremental.digest() == value.digest())
    original = factory(b'prefix')
    copied = original.copy()
    original.update(bytearray(b'first'))
    copied.update(b'second')
    print(original.hexdigest(), copied.hexdigest())
    print(original.digest() == original.digest())
""",
    )
    assert result.stderr == ""


def test_md5_vectors_padding_updates_copies_and_buffers(tmp_path):
    compare(
        tmp_path,
        """
from _md5 import md5
for data in (b'', b'a', b'abc', b'message digest', b'abcdefghijklmnopqrstuvwxyz',
             bytes(range(256)), b'x' * 55, b'x' * 56, b'x' * 63, b'x' * 64, b'x' * 65):
    digest = md5(data)
    print(digest.name, digest.digest_size, digest.block_size, digest.hexdigest())
    incremental = md5(usedforsecurity=False)
    for offset in range(0, len(data), 7):
        incremental.update(data[offset:offset + 7])
    print(incremental.digest() == digest.digest(), incremental.hexdigest() == digest.hexdigest())
digest = md5(b'prefix')
branch = digest.copy()
digest.update(b'left')
branch.update(b'right')
print(digest.hexdigest(), branch.hexdigest())
print(digest.digest() == digest.digest())
digest.update(bytearray(b'array'))
digest.update(memoryview(b'view'))
print(digest.hexdigest())
for value in ('text', 42, [1, 2]):
    try:
        md5(value)
    except TypeError as error:
        print(type(error).__name__, str(error))
""",
    )
