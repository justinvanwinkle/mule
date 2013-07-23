# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import sre_parse
import sre_compile
import sys

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

        scanner = Scanner(lexicon)
        scan = scanner.scan(self.code)
        if scan[1]:
            raise Exception(scan[1])
        return [('MODULE', ''), ('BLOCK', '')] + scan[0] + [('ENDBLOCK', '')]

    def feed(self):
        if self.token_position < len(self.tokens):
            kind, value = self.tokens[self.token_position]
            token_handler = self.token_map[kind]
            self.token_position += 1
            self.token_handler = token_handler
            self.token_value = value
            if debug:
                print >> sys.stderr, 'Feeding %s :: %s' % (
                    token_handler, value)

    def maybe_match(self, token_name):
        if token_name == self.token_handler.name:
            self.feed()
            return True
        return False

    def match(self, token_name=None):
        if token_name != self.token_handler.name:
            raise SyntaxError('Expected %s, Got %s' % (
                token_name, self.token_handler.name))
        if debug:
            print >> sys.stderr, 'MATCHED: %s' % token_name
        self.feed()

    def watch(self, token_name):
        if token_name == self.token_handler.name:
            if debug:
                print >> sys.stderr, 'SAW A %s' % token_name
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
        if debug:
            print >> sys.stderr, 'starting with left %s' % left
            print >> sys.stderr, 'looping? %s rbp %s lbp %s' % (
                self.token_handler,
                rbp,
                self.token_handler.lbp)
        while rbp < self.token_handler.lbp:
            t = self.token_handler
            self.feed()
            left = t.led(self, left)
            if debug:
                print >> sys.stderr, 'left is %s' % left
                print >> sys.stderr, 'rbp %s lbp %s' % (
                    rbp, self.token_handler.lbp)

        return left


class PyclParser(Parser):
    def __init__(self, code):
        Parser.__init__(self, code)
        for op in [
                Module(),
                Block(),
                EndBlock(),
                Class(),
                Def(),
                For(),
                If(),
                While(),
                In(),
                Return(),
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
                new_tokens.append(('BLOCK', ''))
            elif new_indent < current_indent:
                for _ in range(current_indent - new_indent):
                    new_tokens.append(('ENDBLOCK', ''))

        for index, (name, value) in enumerate(tokens):
            new_indent = current_indent
            if name == 'WHITESPACE':
                if not is_significant(index):
                    continue
                new_indent = len(value) / 4
            elif name == 'NEWLINE':
                if prev_tok(index) == 'NEWLINE':
                    continue
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


class LispPackage(LispNode):
    kind = 'package'

    def __init__(self, block):
        self.block = block

    def __repr__(self):
        return '(module \n%s)' % self.block


class Module(Op):
    lbp = 100
    name = 'MODULE'

    def nud(self, parser, value):
        return LispPackage(parser.expression(0))


class LispBody(LispNode):
    def __init__(self, forms):
        self.forms = forms

    def __repr__(self):
        return '\n'.join(repr(x) for x in self.forms)


class Block(Op):
    lbp = 10
    name = 'BLOCK'

    def nud(self, parser, value):
        forms = []
        while parser.watch('ENDBLOCK'):
            forms.append(parser.expression(10))
            parser.maybe_match('NEWLINE')
        return LispBody(forms)


class EndBlock(Op):
    lbp = 0
    name = 'ENDBLOCK'


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

    def __init__(self, name, arg_names, body):
        self.name = name
        self.arg_names = arg_names
        self.body = body

    def __repr__(self):
        return '(defun %s (%s) \n%s)' % (
            self.name,
            ' '.join('%s' % x for x in self.arg_names),
            self.body)


class Def(Op):
    lbp = 10
    regex = 'def'
    name = 'DEF'

    def nud(self, parser, value):
        name = parser.expression(10)
        parser.match('LPAREN')
        arg_names = []
        while parser.watch('RPAREN'):
            arg_names.append(parser.expression(10))
            parser.maybe_match('COMMA')
        parser.match('COLON')
        parser.match('NEWLINE')

        body = parser.expression(10)
        return DefunNode(name, arg_names, body)


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


class ReturnNode(LispNode):
    def __init__(self, return_expr):
        self.return_expr = return_expr

    def __repr__(self):
        return '(return %s)' % self.return_expr


class Return(Op):
    lbp = 80
    regex = 'return'
    name = 'RETURN'

    def nud(self, parser, value):
        return ReturnNode(parser.expression(80))


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


class TupleNode(LispNode):
    kind = 'tuple'

    def __init__(self, values):
        self.values = values

    def __repr__(self):
        return '(tuple %s)' % ' '.join(
            '%s' % x for x in self.values)


class LParen(Op):
    lbp = 0
    regex = r'\('
    name = 'LPAREN'

    def led(self, parser, left):
        if left.kind == 'symbol':
            return left

    def nud(self, parser, value):
        values = []
        while parser.watch('RPAREN'):
            values.append(parser.expression())
            parser.maybe_match('COMMA')

        if len(values) > 1:
            return TupleNode(values)
        return values[0]


class RParen(Op):
    lbp = 0
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
    lbp = 0
    regex = ','
    name = 'COMMA'


class AssignNode(LispNode):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return '(assign %s %s)' % (self.left, self.right)


class Assign(Op):
    lbp = 20
    regex = '='
    name = 'ASSIGN'

    def led(self, parser, left):
        return AssignNode(left, parser.expression(20))


class Colon(Op):
    lbp = 0
    regex = ':'
    name = 'COLON'


class NumberNode(LispNode):
    kind = 'number'

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return '(number %s)' % self.value


class Float(Op):
    lbp = 40
    regex = r"-?[0-9]+\.[0-9]+([eE]-?[0-9]+)?"
    name = 'FLOAT'


class Integer(Op):
    lbp = 40
    regex = r"-?[0-9]+"
    name = 'INTEGER'

    def nud(self, parser, value):
        return NumberNode(value)


class BinaryOperatorNode(LispNode):
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def __repr__(self):
        return '(%s %s %s)' % (self.op, self.left, self.right)


class Plus(Op):
    lbp = 110
    regex = r'\+'
    name = 'PLUS'

    def led(self, parser, left):
        return BinaryOperatorNode('+', left, parser.expression(110))


class Minus(Op):
    lbp = 110
    regex = r'\-'
    name = 'MINUS'

    def led(self, parser, left):
        return BinaryOperatorNode('-', left, parser.expression(110))


class Multiply(Op):
    lbp = 120
    regex = r'\*'
    name = 'MULTIPLY'

    def led(self, parser, left):
        return BinaryOperatorNode('*', left, parser.expression(110))


class Divide(Op):
    lbp = 120
    regex = r'\/'
    name = 'DIVIDE'

    def led(self, parser, left):
        return BinaryOperatorNode('/', left, parser.expression(110))


class String(Op):
    lbp = 0
    regexes = [r"'''(.*?)'''",
               r"'''(.*?)'''",
               r"'(.*?)'",
               r'"(.*?)"']
    name = 'STRING'


class Newline(Op):
    lbp = 0
    regex = r'\n'
    name = 'NEWLINE'


class Whitespace(Op):
    regex = r'[ ]+'
    name = 'WHITESPACE'
