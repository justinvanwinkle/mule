"""Codec registry caching and normal text codecs use compiled search functions."""

import pytest

from test.mule.compiler.test_exception_groups import compare


@pytest.mark.integration
def test_builtin_aliases_and_custom_registry_cache_invalidation(tmp_path):
    compare(
        tmp_path,
        r"""
import codecs
for encoding in ('utf-8', 'UTF--8', 'ascii', 'latin1', 'utf-16-le'):
    info = codecs.lookup(encoding)
    print(encoding, info.name, info is codecs.lookup(encoding))
print(codecs.encode('héllo', 'utf-8'))
print(codecs.decode(b'h\xc3\xa9llo', 'utf-8'))
calls = []
def encode(value, errors='strict'):
    return 'encoded:' + value, len(value)
def decode(value, errors='strict'):
    return 'decoded:' + value, len(value)
entry = codecs.CodecInfo(encode, decode, name='custom')
def search(name):
    calls.append(name)
    return entry if name == 'mule_custom_codec' else None
codecs.register(search)
print(codecs.lookup('Mule--Custom codec') is entry)
print(codecs.lookup('mule_custom_codec') is entry, calls)
print(codecs.encode('text', 'mule-custom-codec'))
print(codecs.decode('text', 'mule-custom-codec'))
codecs.unregister(search)
try:
    codecs.lookup('mule-custom-codec')
except LookupError:
    print('unregistered')
for value in (42, None):
    try:
        codecs.register(value)
    except TypeError as error:
        print(str(error))
for value in (42, b'utf8', 'nul\x00name'):
    try:
        codecs.lookup(value)
    except (TypeError, ValueError) as error:
        print(type(error).__name__)
def broken(name):
    return 42 if name == 'mule_invalid_codec' else None
codecs.register(broken)
try:
    codecs.lookup('mule-invalid-codec')
except TypeError as error:
    print(str(error))
codecs.unregister(broken)
""",
    )


@pytest.mark.integration
def test_incremental_utf8_buffers_only_valid_incomplete_suffixes(tmp_path):
    compare(
        tmp_path,
        r"""
import codecs
decoder = codecs.getincrementaldecoder('utf8')()
for chunk in (b'a\xe2', b'\x82', b'\xac\xf0\x9f', b'\x92\xa9z'):
    print(repr(decoder.decode(chunk)), decoder.getstate())
print(repr(decoder.decode(b'', final=True)), decoder.getstate())
for data in (b'\xc2', b'\xe2\x82', b'\xf0\x9f\x92', b'\xe0\x80', b'\xed\xa0', b'\xf4\x90'):
    for final in (False, True):
        try:
            print(codecs.utf_8_decode(data, 'strict', final))
        except UnicodeDecodeError:
            print('invalid', final)
""",
    )
