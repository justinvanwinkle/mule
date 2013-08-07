# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function
import sre_parse
import sre_compile
from sre_constants import BRANCH, SUBPATTERN

from sys import stderr


class Scanner(object):
    def __init__(self, lexicon, flags=0):
        self.lexicon = lexicon
        # combine phrases into a compound pattern
        p = []
        s = sre_parse.Pattern()
        s.flags = flags
        for phrase, token_type in lexicon:
            if phrase is not None:
                p.append(sre_parse.SubPattern(s, [
                    (SUBPATTERN, (len(p)+1, sre_parse.parse(phrase, flags))),
                ]))
        s.groups = len(p)+1
        p = sre_parse.SubPattern(s, [(BRANCH, (None, p))])
        self.scanner = sre_compile.compile(p)

    def scan(self, string):
        result = []
        append = result.append
        match = self.scanner.scanner(string).match
        i = 0
        while 1:
            m = match()
            if not m:
                break
            j = m.end()
            if i == j:
                break
            token_type = self.lexicon[m.lastindex-1][1]
            self.match = m
            if token_type is not None:
                append((token_type, m.group()))
            i = j
        return result, string[i:]


class PrattParser(object):
    def __init__(self, code, filename=None):
        self.code = code
        self.filename = filename
        self.registered = []
        self.token_handler = None
        self.token_value = None
        self.token_position = 0
        self.token_map = {}
        self._tokens = None
        self.debug = False

    def __repr__(self):
        return 'Pratt(pos=%s)' % self.token_position

    def log(self, s, *args):
        if self.debug:
            print(s % tuple([repr(x) for x in args]), file=stderr)

    def register(self, op):
        self.registered.append(op)
        self.token_map[op.name] = op

    @property
    def tokens(self):
        if self._tokens is not None:
            return self._tokens
        lexicon = []

        for op in self.registered:
            for regex in op.regexes:
                if regex is None:
                    raise ValueError('None is not a regex')
                lexicon.append((regex, op.name))
        scanner = Scanner(lexicon)
        scan = scanner.scan(self.code)
        if scan[1]:
            raise Exception(scan[1])
        self._tokens = ([('MODULE', ''), ('BLOCK', '')] +
                        scan[0] +
                        [('ENDBLOCK', '')])
        return self._tokens

    def feed(self):
        if self.token_position < len(self.tokens):
            kind, value = self.tokens[self.token_position]
            token_handler = self.token_map[kind]
            self.token_position += 1
            self.token_handler = token_handler
            self.token_value = value
            self.log('Feeding %s :: %s', token_handler, value)

    def maybe_match(self, token_name):
        if token_name == self.token_handler.name:
            self.log('MAYBE-MATCHED: %s', token_name)
            self.feed()
            return True
        return False

    def match(self, token_name=None):
        if token_name != self.token_handler.name:
            raise SyntaxError('Expected %s, Got %s' % (
                token_name, self.token_handler.name))
        self.log('MATCHED: %s', token_name)
        self.feed()

    def watch(self, token_name, consume=True):
        if token_name == self.token_handler.name:
            self.log('WATCH finds %s', token_name)
            if consume:
                self.feed()
            return False
        return True

    def parse(self):
        self.feed()
        return self.expression()

    def expression(self, rbp=0):
        t = self.token_handler
        v = self.token_value
        self.feed()

        left = t.nud(self, v)
        self.log('starting with left %s', left)
        self.log('looping? %s rbp %s lbp %s',
                 self.token_handler,
                 rbp,
                 self.token_handler.lbp)
        while rbp < self.token_handler.lbp:
            t = self.token_handler
            self.log('looped into %s.led! left is %s', t, left)
            self.feed()
            left = t.led(self, left)
            self.log('left is %s', left)
            self.log('rbp %s lbp %s', rbp, self.token_handler.lbp)
        else:
            self.log('no loop')

        return left
