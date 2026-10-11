"""string (Template substitute/safe_substitute, constants, capwords, Formatter) and textwrap (wrap, fill, dedent, indent, shorten)."""
import string
import textwrap
from string import Template

t = Template("$who likes ${what}s; cost: $$$amount")
print("substitute", t.substitute(who="tim", what="kiwi", amount=3))
print("substitute mapping+kw", t.substitute({"who": "ann", "what": "fig"}, amount=5, who="kw wins"))
try:
    t.substitute(who="x")
except KeyError as e:
    print("missing key", type(e).__name__, e.args[0])
print("safe_substitute", t.safe_substitute(who="bo"))
try:
    Template("bad $ sign").substitute()
except ValueError as e:
    print("invalid placeholder", type(e).__name__)
print("safe invalid", Template("bad $ sign $x").safe_substitute(x=1))
print("template attrs", t.template)
print("get_identifiers", Template("$a ${b} $a $$c").get_identifiers())
print("is_valid", Template("$ok").is_valid(), Template("$").is_valid())


class PercentTemplate(Template):
    delimiter = "%"


print("custom delimiter", PercentTemplate("%name is %{age}yo, 100%%").substitute(name="ed", age=7))

print("ascii_letters", string.ascii_letters)
print("lowercase/uppercase", string.ascii_lowercase, string.ascii_uppercase)
print("digits", string.digits, string.hexdigits, string.octdigits)
print("punctuation", string.punctuation)
print("whitespace", repr(string.whitespace))
print("printable len", len(string.printable))
print("capwords", string.capwords("  hello   wORLD  of python "))
print("capwords sep", string.capwords("a-b-c d", "-"))
print("caesar", "Hello".translate(str.maketrans(string.ascii_lowercase, string.ascii_lowercase[3:] + string.ascii_lowercase[:3])))
fmt = string.Formatter()
print("Formatter", fmt.format("{0}-{x:>4}", "a", x=7))
print("Formatter parse", list(fmt.parse("a{b!r:>3}c{}")))

text = "The quick brown fox jumps over the lazy dog and keeps running far away."
print("wrap", textwrap.wrap(text, width=20))
print("fill")
print(textwrap.fill(text, width=25))
print("wrap long word", textwrap.wrap("supercalifragilistic is long", width=8))
print("wrap no break", textwrap.wrap("supercalifragilistic is long", width=8, break_long_words=False))
print("wrap indents", textwrap.wrap("one two three four five", width=12, initial_indent="* ", subsequent_indent="  "))
print("wrap empty", textwrap.wrap(""), textwrap.wrap("   "))
print("wrap max_lines", textwrap.wrap(text, width=20, max_lines=2))
print("wrap hyphen", textwrap.wrap("well-known state-of-the-art", width=10))
print("wrap tabs", textwrap.wrap("a\tb", width=20, tabsize=4))
block = """
    def f():
        return 1
      # odd indent
    """
print("dedent", repr(textwrap.dedent(block)))
print("dedent mixed", repr(textwrap.dedent("  a\n\tb\n")))
print("dedent blank lines", repr(textwrap.dedent("    x\n\n    y\n")))
print("indent", repr(textwrap.indent("a\n\nb\n", "> ")))
print("indent predicate", repr(textwrap.indent("a\n\nb", "+", lambda line: True)))
print("shorten", textwrap.shorten(text, width=30))
print("shorten placeholder", textwrap.shorten("Hello  world!", width=12, placeholder="..."))
print("shorten fits", textwrap.shorten("  lots   of   space  ", width=50))
w = textwrap.TextWrapper(width=15, fix_sentence_endings=True)
print("TextWrapper", w.wrap("End. Next one is here. Done."))
