# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import sre_parse
import sre_compile
import sys

from werkzeug import cached_property

debug = True


def log(s, *args):
    print >> sys.stderr, s % tuple([repr(x) for x in args])


def clmap(forms):
    return [x.cl() for x in forms]


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
        self.state = {}

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
                log('Feeding %s :: %s', token_handler, value)

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
            log('MATCHED: %s', token_name)
        self.feed()

    def watch(self, token_name):
        if token_name == self.token_handler.name:
            if debug:
                log('SAW A %s', token_name)
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
            log('starting with left %s', left)
            log('looping? %s rbp %s lbp %s',
                self.token_handler,
                rbp,
                self.token_handler.lbp)
        while rbp < self.token_handler.lbp:
            if debug:
                log('looping!')
            t = self.token_handler
            self.feed()
            left = t.led(self, left)
            if debug:
                log('left is %s', left)
                log('rbp %s lbp %s', rbp, self.token_handler.lbp)
        else:
            if debug:
                log('no loop')

        return left


class Namespace(object):
    def __init__(self, return_name=None, current_class=None):
        self.s = set()
        self.let_count = 0
        self.return_name = return_name
        self.current_class = current_class

    def add(self, name):
        self.s.add(name)

    def __contains__(self, name):
        return name in self.s


class NamespaceStack(object):
    def __init__(self):
        self.stack = []
        self.cns = None

    @property
    def return_name(self):
        for ns in reversed(self.stack):
            if ns.return_name is not None:
                return ns.return_name

    @property
    def depth(self):
        return len(self.stack) - 1

    def push_new(self, return_name=None):
        ns = Namespace(return_name=return_name)
        self.cns = ns
        self.stack.append(ns)

    def pop(self):
        lost_namespace = self.stack.pop()
        if self.depth > 0:
            self.cns = self.stack[-1]
        return lost_namespace

    def add(self, name):
        self.cns.add(name)

    def __contains__(self, name):
        if debug:
            log('checking NS for %s', name)
        for ns in self.stack:
            if name in ns:
                if debug:
                    log('found in NS')
                return True
        if debug:
            log('not found')
        return False


class PyclParser(Parser):
    def __init__(self, code):
        Parser.__init__(self, code)
        self.ns = NamespaceStack()
        for op in [
                Module(),
                Block(),
                EndBlock(),
                Pass(),
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
                Equality(),
                Assign(),
                Colon(),
                Float(),
                Integer(),
                Dot(),
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
                if prev_tok(pos) != 'NEWLINE':
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
                if prev_tok(index) != 'NEWLINE':
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

    def __repr__(self):
        return '|%s|' % self.name


class LispNode(object):
    kind = 'node'

    def __repr__(self):
        return '(%s XXXXXXXXXX)' % self.kind

    def cl(self):
        return '(%s-XXX XXX)' % self.kind


class LispPackage(LispNode):
    kind = 'package'

    def __init__(self, block):
        self.block = block

    def __repr__(self):
        return '(package \n%s)' % self.block

    def cl(self):
        return self.block.cl(implicit_body=True)


class Module(Op):
    lbp = 100
    name = 'MODULE'

    def nud(self, parser, value):
        parser.ns.push_new()
        package = LispPackage(parser.expression(0))
        parser.ns.pop()
        return package


class LispBody(LispNode):
    kind = 'body'

    def __init__(self, forms):
        self.forms = forms

    def __repr__(self):
        return '\n'.join(repr(x) for x in self.forms)

    def cl(self, implicit_body=False):
        if implicit_body:
            return '\n'.join(clmap(self.forms))
        elif len(self.forms) > 1 and not implicit_body:
            return '(PROGN %s)' % '\n'.join(clmap(self.forms))
        elif len(self.forms) == 1:
            return self.forms[0].cl()
        else:
            # Do we need this?
            return 'nil'


def parse_rest_of_block(parser):
    forms = []
    while parser.token_handler.name != 'ENDBLOCK':
        while parser.maybe_match('NEWLINE'):
            pass
        form = parser.expression(10)
        forms.append(form)
        while parser.maybe_match('NEWLINE'):
            pass
    return forms


class Block(Op):
    lbp = 10
    name = 'BLOCK'

    def nud(self, parser, value):
        forms = parse_rest_of_block(parser)
        parser.match('ENDBLOCK')
        return LispBody(forms)


class EndBlock(Op):
    lbp = 0
    name = 'ENDBLOCK'


class NilNode(LispNode):
    kind = 'nil'

    def __repr__(self):
        return 'pass'

    def cl(self):
        return 'NIL'


class Pass(Op):
    lbp = 0
    regex = 'pass'
    name = 'PASS'

    def nud(self, parser, value):
        return NilNode()


class MethodNode(LispNode):
    kind = 'defun'

    def __init__(self, name, class_name, arg_names, body):
        self.name = name
        self.class_name = class_name
        self.arg_names = arg_names
        self.body = body

    def __repr__(self):
        return '(method %s %s (%s) \n%s)' % (
            self.name,
            self.class_name,
            ' '.join('%s' % x for x in self.arg_names),
            self.body)

    def cl(self):
        return '(DEFMETHOD %s ((%s %s) %s) %s)' % (
            self.name.cl(),
            self.arg_names[0].cl(),
            self.class_name.cl(),
            ' '.join(clmap(self.arg_names[1:])),
            self.body.cl(implicit_body=True))


class CLOSClassNode(LispNode):
    kind = 'class'

    def __init__(self, name, bases=(), slots=(), members=(), methods=()):
        self.name = name
        self.bases = list(bases)
        self.slots = list(slots)
        self.members = list(members)
        self.methods = list(methods)

    def add_form(self, form):
        if form.kind == 'defun':
            self.methods.append(form)

    def __repr__(self):
        return '(class %s (%s) (%s) (%s))' % (
            self.name,
            self.bases,
            self.members,
            self.methods)

    def cl_method(self, defun):
        return MethodNode(defun.name, self.name, defun.arg_names, defun.body)

    def cl_methods(self):
        return '\n'.join(self.cl_method(defun).cl() for defun in self.methods)

    def cl_bases(self):
        return '(%s)' % ' '.join(base.cl() for base in self.bases)

    def cl_slot(self, slot):
        if slot.kind == 'name':
            return slot.cl()
        elif slot.kind == 'assign':
            return '(%s :INITFORM %s)' % (slot.left.cl(), slot.right.cl())

    def cl_slots(self):
        if self.slots:
            return ' '.join(self.cl_slot(slot) for slot in self.slots)
        else:
            return 'NIL'

    def cl(self):
        defclass = '(DEFCLASS %s %s %s)' % (
            self.name.cl(),
            self.cl_bases(),
            self.cl_slots())
        if self.methods:
            return defclass + self.cl_methods()
        return defclass


class Class(Op):
    lbp = 0
    regex = 'class'
    name = 'CLASS'

    def nud(self, parser, value):
        name = parser.expression(100)
        cc = CLOSClassNode(name)
        if parser.maybe_match('LPAREN'):
            while parser.watch('RPAREN'):
                cc.bases.append(parser.expression(10))
                parser.maybe_match('COMMA')
        parser.match('COLON')
        parser.match('NEWLINE')
        body = parser.expression(10)
        for form in body.forms:
            cc.add_form(form)
        return cc


class DefunNode(LispNode):
    kind = 'defun'

    def __init__(self, name, arg_names, body):
        self.name = name
        self.arg_names = arg_names
        self.body = body

    def __repr__(self):
        return '(def %s (%s) \n%s)' % (
            self.name,
            ' '.join('%s' % x for x in self.arg_names),
            self.body)

    def cl(self):
        return '(DEFUN %s (%s) %s)' % (
            self.name.cl(),
            ' '.join(clmap(self.arg_names)),
            self.body.cl(implicit_body=True))


class Def(Op):
    lbp = 10
    regex = 'def'
    name = 'DEF'

    def nud(self, parser, value):
        name = parser.expression(100)
        parser.ns.push_new(return_name=name)
        parser.match('LPAREN')
        arg_names = []
        while parser.watch('RPAREN'):
            arg_names.append(parser.expression(10))
            parser.maybe_match('COMMA')
        parser.match('COLON')
        parser.match('NEWLINE')
        body = parser.expression(10)
        parser.ns.pop()
        return DefunNode(name, arg_names, body)


class ForLoopNode(LispNode):
    def __init__(self, var_name, domain, body):
        self.var_name = var_name
        self.domain = domain
        self.body = body

    def __repr__(self):
        return '(for (in %s %s) \n%s)' % (
            self.var_name,
            self.domain,
            self.body)

    def cl(self):
        return '(LOOP FOR %s BEING THE ELEMENTS OF %s DO %s)' % (
            self.var_name.cl(),
            self.domain.cl(),
            self.body.cl())


class For(Op):
    lbp = 20
    regex = 'for'
    name = 'FOR'

    def nud(self, parser, value):
        var_name = parser.expression(70)
        parser.ns.push_new()
        parser.ns.add(var_name.name)
        parser.match('IN')
        domain = parser.expression(70)
        parser.match('COLON')
        parser.match('NEWLINE')
        body = parser.expression(10)
        parser.ns.pop()
        return ForLoopNode(var_name, domain, body)


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


class InNode(LispNode):
    kind = 'in'

    def __init__(self, thing, collection):
        self.thing = thing
        self.collection = collection

    def __repr__(self):
        return '(IN %s %s)' % (self.thing, self.collection)


class In(Op):
    lbp = 70
    regex = r' in '
    name = 'IN'

    def led(self, parser, left):
        return InNode(left, parser.expression(30))


class ReturnNode(LispNode):
    kind = 'return'

    def __init__(self, return_expr, return_name):
        self.return_expr = return_expr
        self.return_name = return_name

    def __repr__(self):
        return '(return %s)' % self.return_expr

    def cl(self):
        return '(RETURN-FROM %s %s)' % (self.return_name.cl(),
                                        self.return_expr.cl())


class Return(Op):
    lbp = 0
    regex = 'return'
    name = 'RETURN'

    def nud(self, parser, value):
        return ReturnNode(parser.expression(120), parser.ns.return_name)


class SymbolNode(LispNode):
    kind = 'symbol'

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '(symbol %s)' % self.name

    def cl(self):
        return '%s' % self.name


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


class CallNode(LispNode):
    kind = 'call'

    def __init__(self, name, args):
        self.name = name
        self.args = args

    def __repr__(self):
        return '(call %s (%s))' % (
            self.name,
            ' '.join('%s' % x for x in self.args))

    def cl(self):
        return '(%s %s)' % (self.name.cl(), ' '.join(clmap(self.args)))


class LParen(Op):
    lbp = 80
    regex = r'\('
    name = 'LPAREN'

    def led(self, parser, left):
        if left.kind == 'symbol':
            name = left
            args = []
            while parser.watch('RPAREN'):
                args.append(parser.expression(10))
                parser.maybe_match('COMMA')
            return CallNode(name, args)

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


class EqualityNode(LispNode):
    kind = 'equal'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return '(== %s %s)' % (self.left, self.right)

    def cl(self):
        return '(EQUALP %s %s)' % (self.left.cl(), self.right.cl())


class Equality(Op):
    lbp = 40
    regex = '=='
    name = 'EQUALS'

    def led(self, parser, left):
        return EqualityNode(left, parser.expression(60))


class DefParameterNode(LispNode):
    kind = 'defparameter'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return '(assign %s %s)' % (self.left, self.right)

    def cl(self):
        return '(DEFPARAMETER %s %s)' % (self.left.cl(), self.right.cl())


class SetfNode(LispNode):
    kind = 'setf'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return '(assign %s %s)' % (self.left, self.right)

    def cl(self):
        return '(SETF %s %s)' % (self.left.cl(), self.right.cl())


class LetNode(LispNode):
    kind = 'let'

    def __init__(self, left, right, body):
        self.pairs = [(left, right)]
        self.body = body

    def __repr__(self):
        return '(let (%s) %s)' % (
            ' '.join('(%s %s)' % (l, r) for l, r in self.pairs),
            self.body)

    def cl(self):
        return '(LET (%s) %s)' % (
            ' '.join('(%s %s)' % (l.cl(), r.cl()) for l, r in self.pairs),
            self.body.cl())


class Assign(Op):
    lbp = 20
    regex = '='
    name = 'ASSIGN'

    def led(self, parser, left):
        if left.kind == 'attr_lookup' or left.name in parser.ns:
            return SetfNode(left, parser.expression(20))
        elif parser.ns.depth == 0:
            parser.ns.add(left.name)
            return DefParameterNode(left, parser.expression(20))
        else:
            right = parser.expression(10)
            parser.maybe_match('NEWLINE')
            parser.ns.push_new()
            parser.ns.add(left.name)
            rest = parse_rest_of_block(parser)
            let_node = LetNode(left,
                               right,
                               LispBody(rest))
            parser.ns.pop()
            return let_node


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

    def cl(self):
        return '%s' % self.value


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
    kind = 'binary_op'

    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def __repr__(self):
        return '(%s %s %s)' % (self.op, self.left, self.right)

    def cl(self):
        return '(%s %s %s)' % (self.op, self.left.cl(), self.right.cl())


class AttrLookup(LispNode):
    kind = 'attr_lookup'

    def __init__(self, object_name, attribute_name):
        self.object_name = object_name
        self.attribute_name = attribute_name

    def __repr__(self):
        return '(getattr %s %s)' % (self.object_name, self.attribute_name)

    def cl(self):
        return "(SLOT-VALUE %s '%s)" % (self.object_name.cl(),
                                        self.attribute_name.cl())


class Dot(Op):
    lbp = 130
    regex = r'\.'
    name = 'DOT'

    def led(self, parser, left):
        right = parser.expression(50)
        if right.kind == 'call':
            right.args.insert(0, left)
            return right
        return AttrLookup(left, right)


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
