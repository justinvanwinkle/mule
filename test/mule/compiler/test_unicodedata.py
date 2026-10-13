"""Unicode properties and normalization execute without the host runtime."""

import unicodedata

from test.mule.compiler.test_modules import execute


def test_unicode_properties_and_normalization_match_host(tmp_path):
    characters = [
        "\x00",
        "A",
        "\u0301",
        "\u20dd",
        "\ud800",
        "\u0378",
        "界",
        "\uff21",
        "·",
        "\uff76",
        "\U0001f600",
        "\U0010ffff",
    ]
    strings = [
        "",
        "ASCII",
        "e\u0301",
        "\u01fa",
        "A\u0315\u0300",
        "A\u0305\u0301",
        "\u0344",
        "\u1100\u1161\u11a8",
        "\uac01",
        "\u0301\u0327A",
        "\ud800",
        "\ufb03",
        "\u212b",
        "\uff21\u00a0\u2460",
        "\U0001d15e",
    ]
    source = (
        "import unicodedata as u\n"
        "print(u.unidata_version)\n"
        f"for character in {characters!r}:\n"
        " print(u.category(character), u.east_asian_width(character), u.combining(character))\n"
        f"for text in {strings!r}:\n"
        " for form in ['NFC', 'NFD', 'NFKC', 'NFKD']:\n"
        "  print(repr(u.normalize(form, text)))\n"
    )
    expected = [unicodedata.unidata_version]
    expected.extend(
        f"{unicodedata.category(c)} {unicodedata.east_asian_width(c)} {unicodedata.combining(c)}"
        for c in characters
    )
    expected.extend(
        repr(unicodedata.normalize(form, text))
        for text in strings
        for form in ("NFC", "NFD", "NFKC", "NFKD")
    )
    result = execute(tmp_path, {}, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == expected


def test_unicode_argument_validation_and_string_subclasses(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import unicodedata as u
for function in [u.category, u.east_asian_width, u.combining]:
    for value in ['', 'ab', 1, None, b'a']:
        try:
            function(value)
        except TypeError:
            print('character error')
for form, text in [('wrong', 'a'), (None, ''), ('wrong', None), ('NFC', b'a')]:
    try:
        u.normalize(form, text)
    except ValueError:
        print('form error')
    except TypeError:
        print('type error')
class Text(str):
    def __len__(self):
        raise RuntimeError('length override')
    def __iter__(self):
        raise RuntimeError('iteration override')
    def __getitem__(self, index):
        raise RuntimeError('index override')
    def __eq__(self, other):
        raise RuntimeError('equality override')
print(u.category(Text('A')))
print(u.east_asian_width(Text('A')), u.combining(Text('A')))
print(u.normalize(Text('NFC'), Text('e\\u0301')))
print(repr(u.normalize(Text('NFD'), Text('é'))))
print(repr(u.normalize(Text('wrong'), Text(''))))
try:
    u.normalize(Text('wrong'), Text('a'))
except ValueError:
    print('subclass form error')
for value in [Text(''), Text('ab')]:
    try:
        u.category(value)
    except TypeError:
        print('subclass character error')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "character error\n" * 15
        + "form error\ntype error\ntype error\ntype error\nLu\nNa 0\né\n"
        + repr(unicodedata.normalize("NFD", "é"))
        + "\n''\nsubclass form error\n"
        + "subclass character error\nsubclass character error\n"
    )
