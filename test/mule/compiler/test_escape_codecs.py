"""Compiled escape codecs use Python 3.14 as the conversion oracle."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_unicode_escape_storage_roundtrips_and_decode_errors(tmp_path):
    compare(
        tmp_path,
        r"""
for encoding in ('unicode_escape', 'raw_unicode_escape'):
    for text in ('', 'hello', chr(34) + chr(39) + chr(92), chr(10) + chr(9) + chr(13) + chr(0) + chr(127), 'éĀ💩', chr(0xd800)):
        encoded = text.encode(encoding)
        print(encoding, encoded, encoded.decode(encoding) == text)
    for data in (b'abc\xff', b'\\n\\t\\x41\\101', b'\\u0041\\U0001f4a9',
                 b'\\u0041', b'\\u12', b'\\u12z4', b'\\U00110000', b'\\'):
        for errors in ('strict', 'ignore', 'replace', 'backslashreplace'):
            try:
                print(encoding, data, errors, repr(data.decode(encoding, errors)))
            except UnicodeDecodeError as error:
                print(error.encoding, error.object, error.start, error.end, error.reason, str(error))
for error in (UnicodeEncodeError('sample', 'aé💩', 0, 1, 'why'),
              UnicodeEncodeError('sample', 'aé💩', 1, 3, 'why'),
              UnicodeDecodeError('sample', b'abc', 1, 2, 'why'),
              UnicodeDecodeError('sample', bytearray(b'abc'), 0, 2, 'why')):
    print(str(error), error.encoding, error.object, error.start, error.end, error.reason)
""",
    )


@pytest.mark.integration
def test_escape_codecs_incremental_consumption_and_error_handlers(tmp_path):
    compare(
        tmp_path,
        r"""
import codecs
import warnings
for encoding in ('unicode_escape', 'raw_unicode_escape'):
    decode = codecs.lookup(encoding).decode
    for data in (b'a\\', b'a\\u12', b'a\\u1234z', b'\\u12g4'):
        for final in (False, True):
            try:
                print(encoding, data, final, decode(data, 'strict', final))
            except UnicodeDecodeError as error:
                print(error.encoding, error.start, error.end, error.reason)
    decoder = codecs.getincrementaldecoder(encoding)()
    for data in (b'a\\', b'u12', b'34z'):
        print(repr(decoder.decode(data)), decoder.getstate())
    print(repr(decoder.decode(b'', final=True)))
def replace(error):
    print(error.encoding, error.start, error.end, error.reason)
    return '[invalid]', error.end
codecs.register_error('escape_test', replace)
print(codecs.unicode_escape_decode(b'\\u12z4', 'escape_test', True))
with warnings.catch_warnings(record=True) as recorded:
    warnings.simplefilter('always', DeprecationWarning)
    print(codecs.unicode_escape_decode(b'\\q\\z', 'strict', True))
    print(codecs.unicode_escape_decode(b'\\777', 'strict', True))
    print([(warning.category.__name__, str(warning.message)) for warning in recorded])
""",
    )
