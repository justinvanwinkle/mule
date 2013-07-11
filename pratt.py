# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import sre_parse
import sre_compile


class Scanner(object):
    def __init__(self, lexicon, flags=0):
        from sre_constants import BRANCH, SUBPATTERN
        self.lexicon = lexicon
        # combine phrases into a compound pattern
        p = []
        s = sre_parse.Pattern()
        s.flags = flags
        for phrase, action in lexicon:
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
            action = self.lexicon[m.lastindex-1][1]
            if hasattr(action, '__call__'):
                self.match = m
                action = action(self, m.group())
            if action is not None:
                append(action)
            i = j
        return result, string[i:]


def tok(token_type):
    if token_type is None:
        return None

    def _(scanner, token):
        return token_type, token
    return _


def tokenize(code):
    return PyclParser(code).tokens


def tokenize_file(fn):
    with open(fn) as f:
        return tokenize(f.read())


class Parser(object):
    def __init__(self, code):
        self.code = code
        self.registered = []
        self.token = None

    def register(self, op):
        self.registered.append(op)

    @property
    def tokens(self):
        lexicon = []

        for op in self.registered:
            if op.regex is not None:
                lexicon.append((op.regex, tok(op.name)))
            elif op.regexes is not None:
                for regex in op.regexes:
                    if regex is None:
                        raise ValueError('None is not a regex')
                    lexicon.append((regex, tok(op.name)))
            else:
                raise Exception('No regex for op %s' % op.name)

        scanner = Scanner(lexicon)
        scan = scanner.scan(self.code)
        if scan[1]:
            raise Exception(scan[1])
        return scan[0]

    def next(self):
        for type, token in self.tokens:
            self.token = type, token
            yield None

    def match(self, tok=None):
        if tok is not None and tok != type(self.token):
            raise SyntaxError('Expected %s' % tok)
        self.next()

    def parse(self):
        self.next()
        return self.expression()

    def expression(self, rbp=0):
        t = self.token
        self.next()
        left = t.nud()
        while rbp < self.token.lbp:
            t = self.token
            self.next()
            left = t.led(left)
        return left


class PyclParser(Parser):
    def __init__(self, code):
        Parser.__init__(self, code)
        self.registered = [
            Class(),
            Def(),
            For(),
            While(),
            Name(),
            LParen(),
            RParen(),
            LBracket(),
            RBracket(),
            LBrace(),
            RBrace(),
            Comma(),
            Assign(),
            Colon(),
            Float(),
            Integer(),
            Plus(),
            Minus(),
            Multiply(),
            Divide(),
            String(),
            Newline(),
            Whitespace()]

    @staticmethod
    def handle_whitespace(tokens):
        def is_significant(pos):
            if pos > 0:
                prev_token = tokens[pos - 1]
                if prev_token[0] != 'NEWLINE':
                    return False
            else:
                return False

            next_token = tokens[pos + 1]
            if next_token[0] != 'NEWLINE':
                return True
            return False

        new_tokens = []
        current_indent = 0
        for index, (name, value) in enumerate(tokens):
            if name == 'WHITESPACE':
                if not is_significant(index):
                    continue

                new_indent = len(value) / 4
                if new_indent > current_indent + 1:
                    raise Exception('Indented too much %s' % index)
                elif new_indent == current_indent + 1:
                    new_tokens.append(('INDENT', ''))
                if new_indent < current_indent:
                    for _ in range(current_indent - new_indent):
                        new_tokens.append(('DEDENT', ''))
                current_indent = new_indent
            else:
                new_tokens.append((name, value))
        return new_tokens

    @property
    def tokens(self):
        return self.handle_whitespace(super(PyclParser, self).tokens)



class Op(object):
    lbp = 10
    regex = None
    regexes = None
    name = None


class Class(Op):
    lbp = 10
    regex = 'class'
    name = 'CLASS'

    def nud(self):
        return expression(100)

    def led(self, left):
        right = expression(10)
        return left + right


class Def(Op):
    lbp = 10
    regex = 'def'
    name = 'DEF'

    def nud(self):
        return -expression(100)

    def led(self, left):
        return left - expression(10)


class For(Op):
    lbp = 10
    regex = 'for'
    name = 'FOR'


class While(Op):
    lbp = 10
    regex = 'while'
    name = 'WHILE'

    def led(self, left):
        return left * expression(20)


class Name(Op):
    lbp = 20
    regex = r"[^\W\d]\w*"
    name = 'NAME'

    def led(self, left):
        return left / expression(20)


class LParen(Op):
    lbp = 40
    regex = r'\('
    name = 'LPAREN'


class RParen(Op):
    lbp = 40
    regex = r'\)'
    name = 'RPAREN'


class LBracket(Op):
    lbp = 40
    regex = r'\['
    name = 'LBRACKET'


class RBracket(Op):
    lbp = 40
    regex = r'\]'
    name = 'RBRACKET'


class LBrace(Op):
    lbp = 40
    regex = r'\{'
    name = 'LBRACE'


class RBrace(Op):
    lbp = 40
    regex = r'\}'
    name = 'RBRACE'


class Comma(Op):
    lbp = 10
    regex = ','
    name = 'COMMA'


class Assign(Op):
    lbp = 50
    regex = '='
    name = 'ASSIGN'


class Colon(Op):
    lbp = 10
    regex = ':'
    name = 'COLON'


class Float(Op):
    lbp = 10
    regex = r"-?[0-9]+\.[0-9]+([eE]-?[0-9]+)?"
    name = 'FLOAT'


class Integer(Op):
    lbp = 10
    regex = r"-?[0-9]+"
    name = 'INTEGER'


class Plus(Op):
    lbp = 10
    regex = r'\+'
    name = 'PLUS'


class Minus(Op):
    lbp = 10
    regex = r'\-'
    name = 'MINUS'


class Multiply(Op):
    lbp = 20
    regex = r'\*'
    name = 'MULTIPLY'


class Divide(Op):
    lbp = 20
    regex = r'\/'
    name = 'DIVIDE'


class String(Op):
    lbp = 20
    regexes = [r"'''(.*?)'''",
               r"'''(.*?)'''",
               r"'(.*?)'",
               r'"(.*?)"']
    name = 'STRING'


class Newline(Op):
    lbp = 10
    regex = r'\n'
    name = 'NEWLINE'


class Whitespace(Op):
    regex = r'[ ]+'
    name = 'WHITESPACE'
