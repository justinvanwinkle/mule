#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function

import string

from pratt import PrattParser

atoms = """\
~ |= | ^= ^ >>= >> >= > == <> <= <<= << < /= //= \
// / -= - += + *= **= ** * &= & %= % != , @ { } \
( ) , : ' \ [ ] " `""".split()


class MuleTokenizer(PrattParser):
    def __init__(self, code, char_defs):
        super(MuleTokenizer, self).__init__(code)
        self.char_map = {}

        for char_def in char_defs:
            self.register(char_def())

    def register(self, char_def):
        super(MuleTokenizer, self).register(char_def)
        for c in char_def.chars:
            if c in self.char_map:
                raise Exception('duplicate %s' % c)
            self.char_map[c] = char_def

    @property
    def tokens(self):
        tokens = []
        for c in self.code:
            name = self.char_map[c].name
            tokens.append((name, c))

        return tokens


all_chars = []


def register_char(cls):
    all_chars.append(cls)
    return cls


class Char(object):
    name = None
    chars = None
    lbp = 0

    def __repr__(self):
        return '%s-char' % self.name


# @register_char
# class File(Char):
#     name = 'FILE'
#     chars = set()
#     lbp = 0

#     def nud(self, parser, c):
#         tokens = []
#         while parser.watch('ENDFILE'):
#             token = parser.expression()
#             tokens.append(token)
#             parser.log('TOKENS %s' % tokens)

#         return tokens


@register_char
class EnumChar(Char):
    lbp = 110
    name = 'enum'
    chars = set(''.join(atoms) + '()[]{}<>')
    _prefix_map = set()

    for atom in atoms:
        for l in range(1, len(atom) + 1):
            _prefix_map.add(atom[:l])

    for char in chars:
        if char not in _prefix_map:
            print('MISSING', char)

    def nud(self, parser, c):
        if c not in self._prefix_map:
            raise Exception('%r - WHAT??? WHAT!?!?' % c)
        right = parser.expression(109)
        return Token('op', c + right.value)

    def led(self, parser, left):
        test_val = left.value + parser.token_value
        if test_val in self._prefix_map:
            left.value = test_val
            return left
        return Token('op', parser.token_value, left=left)


@register_char
class DigitChar(Char):
    lbp = 50
    name = 'digit'
    chars = set(string.digits)

    def nud(self, parser, c):
        return Token('number', c, parser.expression(49))

    def lun(self, parser, left):
        if left.kind in ('dot', 'number'):
            left.value += parser.token_value
            return left


@register_char
class DotChar(Char):
    lbp = 40
    chars = {'.'}
    name = 'dot'


@register_char
class WordChar(Char):
    lbp = 100
    name = 'word'
    chars = set(string.letters) | {'_'}

    def nud(self, parser, c):
        parser.log('nud')
        right = parser.expression(99)
        return Token('word', c, right=right.value)

    def led(self, parser, left):
        parser.log('led')
        left.value += parser.token_value
        return left


@register_char
class NewlineChar(Char):
    lbp = 100
    name = 'newline'
    chars = {'\n'}

    def nud(self, parser, c):
        right = parser.expression()
        return Token('newline', c, right.value)

    def led(self, parser, left):
        return Token('newline',
                     parser.token_value,
                     left=left,
                     right=parser.expression())


@register_char
class Space(Char):
    lbp = 100
    name = 'space'
    chars = {' '}

    def nud(self, parser, c):
        return Token('space', c)

    def led(self, parser, left):
        return Token('space', ' ', left=left, right=parser.expression())


class Token(object):
    def __init__(self, kind, value, left=None, right=None):
        self.kind = kind
        self.value = value
        self.left = left
        self.right = right

    def __repr__(self):
        return '|%r|-%s' % (self.value, self.kind)


def unbake_tokens(tok):
    toks = []
    while tok is not None:
        toks.append(tok.value)
        tok = tok.next
    return toks

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

    try:
        mule_tokenizer = MuleTokenizer(code, all_chars)
        mule_tokenizer.debug = True
        result = mule_tokenizer.parse()
    except Exception:
        import sys
        import tracebackturbo
        print(tracebackturbo.format_exc(with_vars=True))
        sys.exit(1)

    print()
    print(unbake_tokens(result))
