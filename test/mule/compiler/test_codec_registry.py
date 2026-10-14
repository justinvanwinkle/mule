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


@pytest.mark.integration
def test_registered_text_methods_bom_state_buffers_and_callback_contracts(tmp_path):
    compare(
        tmp_path,
        r"""
import codecs
import warnings
for encoding in ('utf-8-sig', 'UTF_8_SIG'):
    encoded = 'snow 雪'.encode(encoding)
    print(encoding, encoded, encoded.decode(encoding),
          bytearray(encoded).decode(encoding), str(encoded, encoding))
for data in (b'plain', b'\xef\xbb\xbftext', b'\xef\xbb\xbf\xef\xbb\xbftext'):
    print(repr(data.decode('utf-8-sig')))
decoder = codecs.getincrementaldecoder('utf-8-sig')()
for chunk in (b'\xef', b'\xbb', b'\xbf\xe9', b'\x9b', b'\xaa'):
    print(repr(decoder.decode(chunk)), decoder.getstate())
print(repr(decoder.decode(b'', final=True)))
encoder = codecs.getincrementalencoder('utf-8-sig')()
print(encoder.encode('a'), encoder.encode('b'), encoder.getstate())
encoder.reset()
print(encoder.encode('c'))
saved = []
def encode(*args):
    print('encoder arguments', len(args))
    return b'encoded', len(args[0])
def decode(*args):
    print('decoder arguments', len(args), type(args[0]).__name__)
    saved.append(args[0])
    return 'decoded', len(args[0])
def search(name):
    return codecs.CodecInfo(encode, decode, name=name) if name == 'text_custom' else None
codecs.register(search)
print('value'.encode('text-custom'))
print('value'.encode('text-custom', 'strict'))
class Data(bytes):
    def __getitem__(self, key): raise AssertionError('slice override')
    def __bytes__(self): raise AssertionError('bytes override')
print(bytes.decode(Data(b'data'), 'text-custom'))
print(b'data'.decode('text-custom', 'strict'))
print(str(b'data', 'text-custom'))
print([view.tobytes() for view in saved])
codecs.unregister(search)
for encoding in ('base64_codec', 'rot_13'):
    for value in ('value', b'value'):
        try:
            value.encode(encoding) if isinstance(value, str) else value.decode(encoding)
        except LookupError as error:
            print(str(error))
def invalid(name):
    if name == 'text_bad_result':
        return codecs.CodecInfo(lambda v, e='strict': ('bad', len(v)),
                               lambda v, e='strict': (b'bad', len(v)), name=name)
    if name == 'text_bad_tuple':
        return codecs.CodecInfo(lambda v, e='strict': 'bad',
                               lambda v, e='strict': ('bad',), name=name)
    if name == 'text_bytearray':
        return codecs.CodecInfo(lambda v, e='strict': (bytearray(b'bytes'), len(v)),
                               lambda v, e='strict': ('text', len(v)), name=name)
codecs.register(invalid)
for encoding in ('text_bad_result', 'text_bad_tuple'):
    for value in ('value', b'value'):
        try:
            value.encode(encoding) if isinstance(value, str) else value.decode(encoding)
        except TypeError as error:
            print(str(error))
with warnings.catch_warnings(record=True) as seen:
    warnings.simplefilter('always')
    print('text'.encode('text_bytearray'))
print([str(item.message) for item in seen])
""",
    )
