#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function

import string

from pratt import PrattParser

atoms = """
~ |= | ^= ^ >>= >> >= > == <> <= <<= << < /= //=
// / -= - += + *= **= ** * &= & %= % != , @ { }
( ) , : ' \" `""".split()


class MuleTokenizer(PrattParser):
    def __init__(self, code, char_defs):
        super(MuleTokenizer, self).__init__(code)
        self.char_map = {}

        for char_def in char_defs:
            self.register(char_def())

    def register(self, char_def):
        super(MuleTokenizer, self).register(char_def)
        for c in char_def.chars:
            self.char_map[c] = char_def

    @property
    def tokens(self):
        tokens = []
        for c in self.code:
            name = self.char_map[c].name
            tokens.append((name, self.token_map[name]))

        return ([('FILE', '')] +
                tokens +
                [('ENDFILE', '')])

all_chars = []


def register_char(cls):
    all_chars.append(cls)
    return cls


class Char(object):
    name = None
    chars = None
    lbp = 0

    def __repr__(self):
        return self.name


@register_char
class File(Char):
    name = 'FILE'
    chars = set()
    lbp = 0

    def nud(self, parser, c):
        tokens = []
        while parser.watch('ENDFILE'):
            token = parser.expression()
            tokens.append(token)
        return tokens


@register_char
class EnumChar(Char):
    lbp = 40
    name = 'enum'
    chars = set(''.join(atoms) + '()[]{}<>')
    _prefix_map = set()
    for atom in atoms:
        for l in range(1, len(atom) + 1):
            _prefix_map.add(atom[:l])

    def nud(self, parser, c):
        assert c in self._prefix_map
        return Token('op', c)

    def led(self, parser, left):
        right = parser.expression(40)
        print(left, right)
        left.value += right.value
        assert left.value in self._prefix_map
        return left


@register_char
class DigitChar(Char):
    name = 'digit'
    chars = set(string.digits)

    def nud(self, parser, c):
        return Token('number', c)

    def lun(self, parser, left):
        right = parser.expression()
        if left.kind in ('dot', 'number'):
            left.value += right.value
            return left


@register_char
class DotChar(Char):
    chars = {'.'}


@register_char
class WordChar(Char):
    name = 'word'
    chars = set(string.letters) | {'_'}

    def nud(self, parser, c):
        return Token('word', c)


@register_char
class NewlineChar(Char):
    name = 'newline'
    chars = {'\n'}

    def nud(self, parser, c):
        return Token('newline', '\n')


@register_char
class Space(Char):
    name = 'space'
    chars = {' '}

    def nud(self, parser, c):
        return Token('space', ' ')


class Token(object):
    def __init__(self, kind, value):
        self.kind = kind
        self.value = value

    def __repr__(self):
        return '|%r|-%s' % (self.value, self.kind)

if __name__ == '__main__':
    import argparse
    from os.path import splitext
    from os.path import split

    argparser = argparse.ArgumentParser(description='Mule Tokenizer')
    argparser.add_argument('mule_fn', help='input file')
    args = argparser.parse_args()

    fn = splitext(split(args.mule_fn)[1])[0]
    with open(args.mule_fn) as f:
        code = f.read()

    mule_tokenizer = MuleTokenizer(code, all_chars)
    result = mule_tokenizer.parse()

    print(result)
