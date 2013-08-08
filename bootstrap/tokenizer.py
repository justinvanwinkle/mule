#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function

import string

enumerated_symbols = """\
~ |= | ^= ^ >>= >> >= > == <> <= <<= << < /= //= \
// / -= - += + *= **= ** * &= & %= % != , @ { } \
( ) , : \ [ ]""".split()


class MuleTokenizer(object):
    def __init__(self, code, char_defs):
        self.code = code
        self.char_map = {}

        for char_def in char_defs:
            self.register(char_def)

    def register(self, char_def):
        for c in char_def.start_chars:
            if c in self.char_map:
                raise Exception('duplicate %s' % c)
            self.char_map[c] = char_def

all_chars = []


def register_token(cls):
    all_chars.append(cls)
    return cls


class Token(object):
    name = None
    start_chars = set()
    rest_chars = set()

    def __init__(self, c):
        self.value = c

    def match(self, c):
        if c in self.rest_chars:
            return True
        return False

    def handle(self, c):
        self.value += c

    def __repr__(self):
        return '|%s|-%s' % (self.value, self.name)


@register_token
class EnumToken(Token):
    start_chars = set(''.join(enumerated_symbols))
    _prefix_map = set()
    atoms = set(enumerated_symbols)

    for atom in atoms:
        for l in range(1, len(atom) + 1):
            _prefix_map.add(atom[:l])

    for char in start_chars:
        if char not in _prefix_map:
            print('MISSING', char)

    def is_prefix(self, s):
        return s in self._prefix_map

    def is_symbol(self, s):
        return s in self.atoms

    @property
    def name(self):
        return self.value

    def match(self, c):
        test_val = self.value + c
        if self.is_prefix(test_val) or self.is_symbol(test_val):
            return True
        return False


@register_token
class NumberToken(Token):
    name = 'number'
    start_chars = set(string.digits)
    rest_chars = start_chars | set('ex')


@register_token
class DotToken(Token):
    start_chars = {'.'}
    name = 'dot'


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
    except Exception:
        import sys
        import tracebackturbo
        print(tracebackturbo.format_exc(with_vars=True))
        sys.exit(1)

    print()
    import pprint
    pprint.pprint([(token.value, token.name)
                   for token in mule_tokenizer.tokens])
