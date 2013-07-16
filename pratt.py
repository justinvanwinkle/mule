# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import sre_parse
import sre_compile

from werkzeug import cached_property

debug = True


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


def parse_file(fn):
    p = PyclParser(open(fn).read().decode('utf8'))
    return p.parse()


class Parser(object):
    def __init__(self, code):
        self.code = code
        self.registered = []
        self.token_handler = None
        self.token_value = None
        self.token_position = 0
        self.token_map = {}

    def register(self, op):
        self.registered.append(op)
        self.token_map[op.name] = op

    @cached_property
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

    def feed(self):
        if self.token_position < len(self.tokens):
            kind, value = self.tokens[self.token_position]
            token_handler = self.token_map[kind]
            self.token_position += 1
            self.token_handler = token_handler
            self.token_value = value
            if debug:
                print 'Feeding %s :: %s' % (token_handler, value)

    def match(self, tok=None):
        if tok is not None and tok != type(self.token):
            raise SyntaxError('Expected %s' % tok)
        self.feed()

    def parse(self):
        self.feed()
        return self.expression()

    def expression(self, rbp=0):
        t = self.token_handler
        v = self.token_value
        self.feed()
        left = t.nud(self, v)
        if debug:
            print 'starting with left %s' % left
            print 'looping? %s rbp %s lbp %s' % (self.token_handler,
                                                 rbp,
                                                 self.token_handler.lbp)
        while rbp < self.token_handler.lbp:
            t = self.token_handler
            self.feed()
            left = t.led(self, left)
            if debug:
                print 'left is %s' % left
                print 'rbp %s lbp %s' % (rbp, self.token_handler.lbp)

        return left


class PyclParser(Parser):
    def __init__(self, code):
        Parser.__init__(self, code)
        for op in [
                Class(),
                Def(),
                For(),
                If(),
                While(),
                In(),
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
                Whitespace()]:
            self.register(op)

    @staticmethod
    def handle_whitespace(tokens):
        def next_tok(pos):
            if pos + 1 >= len(tokens):
                return None
            return tokens[pos + 1][0]

        def prev_tok(pos):
            if pos < 1:
                return None
            return tokens[pos - 1][0]

        def is_significant(pos):
            if pos > 1:
                if prev_tok(pos) != 'NEWLINE' or prev_tok(pos - 1) != 'COLON':
                    return False
            else:
                return False

            if next_tok(pos) != 'NEWLINE':
                return True
            return False

        new_tokens = []
        current_indent = 0

        def change_indent(new_indent):
            if new_indent > current_indent + 1:
                raise Exception('Indented too much %s' % index)
            elif new_indent == current_indent + 1:
                new_tokens.append(('INDENT', ''))
            elif new_indent < current_indent:
                for _ in range(current_indent - new_indent):
                    new_tokens.append(('DEDENT', ''))

        for index, (name, value) in enumerate(tokens):
            new_indent = current_indent
            if name == 'WHITESPACE':
                if not is_significant(index):
                    continue
                new_indent = len(value) / 4
            elif name == 'NEWLINE':
                new_tokens.append((name, value))
                if next_tok(index) not in ('NEWLINE', 'WHITESPACE'):
                    new_indent = 0
            else:
                new_tokens.append((name, value))
            change_indent(new_indent)
            current_indent = new_indent

        return new_tokens

    @cached_property
    def tokens(self):
        return self.handle_whitespace(super(PyclParser, self).tokens)


class Op(object):
    lbp = 10
    regex = None
    regexes = None
    name = None


class LispNode(object):
    kind = 'node'


class Class(Op):
    lbp = 10
    regex = 'class'
    name = 'CLASS'

    def nud(self):
        pass

    def led(self, left):
        pass


class ClassNode(LispNode):
    kind = 'class'


class DefunNode(LispNode):
    kind = 'defun'

    def __init__(self, name, args, body):
        pass


class Def(Op):
    lbp = 10
    regex = 'def'
    name = 'DEF'

    def nud(self):
        return DefunNode()


class For(Op):
    lbp = 20
    regex = 'for'
    name = 'FOR'


class If(Op):
    lbp = 20
    regex = 'if'
    name = 'IF'


class While(Op):
    lbp = 20
    regex = 'while'
    name = 'WHILE'

    def led(self, parser, left):
        return left * parser.expression(20)


class In(Op):
    lbp = 60
    regex = r'in'
    name = 'IN'


class SymbolNode(LispNode):
    kind = 'symbol'

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '(symbol %s)' % self.name


class Name(Op):
    lbp = 0
    regex = r"[^\W\d]\w*"
    name = 'NAME'

    def nud(self, parser, value):
        return SymbolNode(value)


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


class AssignNode(LispNode):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return '(assign %s %s)' % (self.left, self.right)


class Assign(Op):
    lbp = 30
    regex = '='
    name = 'ASSIGN'

    def led(self, parser, left):
        return AssignNode(left, parser.expression(50))


class Colon(Op):
    lbp = 10
    regex = ':'
    name = 'COLON'


class NumberNode(LispNode):
    kind = 'number'

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return '(number %s)' % self.value


class Float(Op):
    lbp = 0
    regex = r"-?[0-9]+\.[0-9]+([eE]-?[0-9]+)?"
    name = 'FLOAT'


class Integer(Op):
    lbp = 0
    regex = r"-?[0-9]+"
    name = 'INTEGER'

    def nud(self, parser, value):
        return NumberNode(value)


class Plus(Op):
    lbp = 110
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
    lbp = 0
    regex = r'\n'
    name = 'NEWLINE'

    def led(self, parser, left):
        return left


class Whitespace(Op):
    regex = r'[ ]+'
    name = 'WHITESPACE'
