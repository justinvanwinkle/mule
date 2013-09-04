# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function


from sys import stderr


class PrattParser(object):
    def __init__(self, code, token_defs, filename=None):
        self.code = code
        self.filename = filename
        self.registered = []
        self.token_handler = None
        self.token_value = None
        self.token_position = 0
        self.tokenize_position = 0
        self.token_map = {}
        self._tokens = []
        self.debug = False
        self._char_dispatch_cache = {}

        for token_def in token_defs:
            self.register(token_def)

    def __repr__(self):
        return 'Pratt(token_position=%s)' % self.token_position

    def log(self, s, *args):
        if self.debug:
            print(s % tuple([repr(x) for x in args]), file=stderr)

    def register(self, token_def):
        self.registered.append(token_def)

    def find_matching_token_def(self, c):
        token_def = self._char_dispatch_cache.get(c)
        if token_def is not None:
            return token_def
        for token_def in self.registered:
            if token_def.can_start(c):
                self._char_dispatch_cache[c] = token_def
                return token_def
        raise Exception('No rule to handle %r' % c)

    def _generate_tokens(self):
        tokens = []
        token = None

        column = 1
        line = 1
        for c in self.code:
            if token and token.match(c):
                token = token.handle(c)
            else:
                if token is not None:
                    complete = token.complete()
                    assert complete
                    tokens.append(token)
                token_def = self.find_matching_token_def(c)
                token = token_def(line=line, column=column)
                token = token.handle(c)

            column += 1
            if c == '\n':
                column = 1
                line += 1

        if token is not None:
            complete = token.complete()
            assert complete
        return tokens

    @property
    def tokens(self):
        if self._tokens:
            return self._tokens
        try:
            tokens = self._generate_tokens()
        except Exception:
            raise #SyntaxError()
        tokens = self._munge_tokens(tokens)
        self._tokens = tokens
        return self._tokens

    def feed(self):
        if self.token_position < len(self.tokens):
            token = self.tokens[self.token_position]
            self.token_position += 1
            self.token_handler = token
            self.token_value = token.value
            self.log('Feeding %s', token)

    def maybe_match(self, token_name):
        if token_name == self.token_handler.name:
            return self.match(token_name)
        return False

    def match(self, token_name=None):
        if token_name != self.token_handler.name:
            raise SyntaxError('Expected %s, Got %s' % (
                token_name, self.token_handler.name))
        self.log('MATCHED: %s', token_name)
        token = self.token_handler
        self.feed()
        return token

    def watch(self, token_name, consume=True):
        if token_name == self.token_handler.name:
            self.log('WATCH finds %s', token_name)
            if consume:
                self.match(token_name)
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
