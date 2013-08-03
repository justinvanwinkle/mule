# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import sre_parse
import sre_compile
import sys

from werkzeug import cached_property

debug = True


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


def parse_file(fn, filename=None):
    p = PyclParser(open(fn).read().decode('utf8'), filename=filename)
    return p.parse()


class Parser(object):
    def __init__(self, code, filename=None):
        self.code = code
        self.filename = filename
        self.registered = []
        self.token_handler = None
        self.token_value = None
        self.token_position = 0
        self.token_map = {}
        self.state = {}

    def log(self, s, *args):
        print >> sys.stderr, '    ' * self.ns.depth + s % tuple(
            [repr(x) for x in args])

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
                self.log('Feeding %s :: %s', token_handler, value)

    def maybe_match(self, token_name):
        if token_name == self.token_handler.name:
            if debug:
                self.log('MAYBE-MATCHED: %s', token_name)
            self.feed()
            return True
        return False

    def match(self, token_name=None):
        if token_name != self.token_handler.name:
            raise SyntaxError('Expected %s, Got %s' % (
                token_name, self.token_handler.name))
        if debug:
            self.log('MATCHED: %s', token_name)
        self.feed()

    def watch(self, token_name, consume=True):
        if token_name == self.token_handler.name:
            if debug:
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
        if debug:
            self.log('starting with left %s', left)
            self.log('looping? %s rbp %s lbp %s',
                     self.token_handler,
                     rbp,
                     self.token_handler.lbp)
        while rbp < self.token_handler.lbp:
            t = self.token_handler
            if debug:
                self.log('looping into %s.led! left is %s', t, left)
            self.feed()
            left = t.led(self, left)
            if debug:
                self.log('left is %s', left)
                self.log('rbp %s lbp %s', rbp, self.token_handler.lbp)
        else:
            if debug:
                self.log('no loop')

        return left


class Namespace(object):
    def __init__(self, return_name=None, class_top_level=False):
        self.s = set()
        self.let_count = 0
        self.return_name = return_name
        self.class_top_level = class_top_level

    def add(self, name):
        self.s.add(name)

    def __contains__(self, name):
        return name in self.s

    def __repr__(self):
        return '<NAMESPACE return_name=%s class_top_level=%s %s>' % (
            self.return_name,
            self.class_top_level,
            ', '.join(self.s))


class NamespaceStack(object):
    def __init__(self):
        self.stack = []

    @property
    def cns(self):
        if self.stack:
            return self.stack[-1]
        else:
            return None

    @property
    def top_level(self):
        return self.depth == 0

    @property
    def class_top_level(self):
        return self.cns.class_top_level

    @property
    def return_name(self):
        for ns in reversed(self.stack):
            if ns.return_name is not None:
                return ns.return_name

    @property
    def depth(self):
        return len(self.stack) - 1

    def push_new(self, return_name=None, class_top_level=False):
        ns = Namespace(return_name=return_name,
                       class_top_level=class_top_level)
        self.stack.append(ns)

    def pop(self):
        lost_namespace = self.stack.pop()
        return lost_namespace

    def add(self, name):
        self.cns.add(name)

    def __contains__(self, name):
        for ns in self.stack:
            if name in ns:
                return True
        return False

    def __repr__(self):
        return '<NamespaceStack depth=%s cns=%s stack=%s>' % (
            self.depth,
            self.cns,
            self.stack)


class PyclParser(Parser):
    def __init__(self, code, filename=None):
        Parser.__init__(self, code, filename=filename)
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
                NoneOp(),
                Return(),
                LParen(),
                RParen(),
                LBracket(),
                RBracket(),
                LBrace(),
                RBrace(),
                Comma(),
                Equality(),
                IncfAssign(),
                Assign(),
                Colon(),
                Float(),
                Integer(),
                Dot(),
                LessThan(),
                Plus(),
                Minus(),
                Multiply(),
                Divide(),
                String(),
                LispLiteral(),
                Name(),
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
        lines = []
        for line in self.code.splitlines(True):
            if not line.strip().startswith('#'):
                lines.append(line)
        self.code = ''.join(lines)
        return self.handle_whitespace(super(PyclParser, self).tokens)

    def parse_rest_of_body(self):
        forms = []
        while self.watch('ENDBLOCK', consume=False):
            while self.maybe_match('NEWLINE'):
                pass
            form = self.expression()
            forms.append(form)
            while self.maybe_match('NEWLINE'):
                pass
        return forms


class Op(object):
    lbp = 0
    regex = None
    regexes = None
    name = None

    def __repr__(self):
        return '|%s|' % self.name


class LispNode(object):
    kind = 'node'

    def __repr__(self):
        return self.cl()

    def cl(self):
        return '(%s-XXX XXX)' % self.kind


class DefpackageNode(LispNode):
    def __init__(self, name):
        self.name = name

    def cl(self):
        return '(DEFPACKAGE "%s" (:USE "CL" "SB-EXT" "SB-C"))' % self.name


class LispPackage(LispNode):
    kind = 'package'

    def __init__(self, module_name, block):
        self.module_name = module_name
        self.block = block

    def cl(self):
        forms = []
        forms.append('(SETF *READTABLE* (COPY-READTABLE NIL))')
        forms.append('(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)')
        forms.append(DefpackageNode(self.module_name).cl())
        forms.append('(IN-PACKAGE "%s")' % self.module_name)
        forms.append(self.block.cl(implicit_body=True))
        return ''.join(forms)


class Module(Op):
    name = 'MODULE'

    def nud(self, parser, value):
        parser.ns.push_new()
        package = LispPackage(parser.filename, parser.expression())
        parser.ns.pop()
        return package


class LispBody(LispNode):
    kind = 'body'

    def __init__(self, forms):
        self.forms = forms

    def cl(self, implicit_body=False):
        if implicit_body:
            return ' '.join(clmap(self.forms))
        elif len(self.forms) > 1 and not implicit_body:
            return '(PROGN %s)' % ' '.join(clmap(self.forms))
        elif len(self.forms) == 1:
            return self.forms[0].cl()
        else:
            # Do we need this?
            return 'nil'


class Block(Op):
    lbp = 0
    name = 'BLOCK'

    def nud(self, parser, value):
        forms = parser.parse_rest_of_body()
        parser.match('ENDBLOCK')
        return LispBody(forms)


class EndBlock(Op):
    name = 'ENDBLOCK'


class NilNode(LispNode):
    kind = 'nil'

    def cl(self):
        return 'NIL'


class Pass(Op):
    regex = 'pass'
    name = 'PASS'

    def nud(self, parser, value):
        return NilNode()


class MethodNode(LispNode):
    kind = 'defun'

    def __init__(self, class_name, defun):
        #import ipdb; ipdb.set_trace()
        self.class_name = class_name
        self.defun = defun
        self.first_arg = defun.arg_names[0]

    def cl(self):
        return '(DEFMETHOD %s ((%s %s) %s) %s)' % (
            self.defun.name,
            self.first_arg,
            self.class_name,
            self.defun.cl_args(skip_first=True),
            self.defun.body.cl(implicit_body=True))


class CLOSClassNode(LispNode):
    kind = 'class'

    def __init__(self, name, bases=(), slots=(), members=(), methods=()):
        self.name = name
        self.bases = list(bases)
        self.slots = list(slots)
        self.members = list(members)
        self.methods = list(methods)
        self.constructor = False

    def add_form(self, form):
        if form.kind == 'defun':
            if form.name.name == 'init':
                self.constructor = form
            self.methods.append(form)
        if form.kind == 'setf':
            if form.left.name == '__slots__':
                for symbol in form.right.values:
                    self.slots.append(symbol.value)

    def cl_method(self, defun):
        return MethodNode(self.name, defun)

    def cl_methods(self):
        return ' '.join(self.cl_method(defun).cl() for defun in self.methods)

    def cl_bases(self):
        return '(%s)' % ' '.join(base.cl() for base in self.bases)

    def cl_slot(self, slot):
        return slot

    def cl_slots(self):
        if self.slots:
            return '(%s)' % ' '.join(self.cl_slot(slot) for slot in self.slots)
        else:
            return 'NIL'

    def cl_init_call(self):
        if not self.constructor:
            return ''
        return '(init self %s)' % self.cl_init_args()

    def cl_init_args(self):
        if not self.constructor:
            return ''
        return ' '.join(x for x in clmap(self.constructor.arg_names[1:]))

    def cl_constructor(self):
        return """(DEFUN %s (%s)
                    (LET ((self (MAKE-INSTANCE \'%s)))
                       %s
                       self))""" % (self.name.cl(),
                                    self.cl_init_args(),
                                    self.name.cl(),
                                    self.cl_init_call())

    def cl(self):
        defclass = '(DEFCLASS %s %s %s)' % (
            self.name.cl(),
            self.cl_bases(),
            self.cl_slots())
        if self.methods:
            defclass += ' '
            defclass += self.cl_methods()
        defclass += ' '
        defclass += self.cl_constructor()

        return defclass


class Class(Op):
    regex = 'class'
    name = 'CLASS'

    def nud(self, parser, value):
        name = parser.expression(80)
        cc = CLOSClassNode(name)
        if parser.maybe_match('('):
            while parser.watch(')'):
                cc.bases.append(parser.expression())
                parser.maybe_match(',')
        parser.match(':')
        parser.match('NEWLINE')
        parser.ns.push_new(class_top_level=True)
        body = parser.expression()
        parser.ns.pop()
        for form in body.forms:
            cc.add_form(form)
        return cc


class DefunNode(LispNode):
    kind = 'defun'

    def __init__(self, name, arg_names, kw_args, body):
        self.name = name
        self.arg_names = arg_names
        self.kw_args = kw_args
        self.body = body

    def cl_kw_args(self):
        if not self.kw_args:
            return ''
        forms = []
        for key, default in self.kw_args:
            forms.append('(%s %s)' % (key, default))
        return '&KEY %s' % ''.join(forms)

    def cl_args(self, skip_first=False):
        forms = []
        splat = ''
        for arg in self.arg_names:
            if arg.kind == 'splat':
                splat = '&REST %s' % arg.right
            else:
                forms.append('%s' % arg)
        if skip_first:
            forms = forms[1:]
        return '%s %s %s' % (
            ' '.join(forms), self.cl_kw_args(), splat)

    def cl(self):
        return '(DEFUN %s (%s) %s)' % (
            self.name,
            self.cl_args(),
            self.body.cl(implicit_body=True))


class FletLambda(DefunNode):
    def __init__(self, defun, right):
        self.defun = defun
        self.right = right

    def cl(self):
        return '(FLET ((%s (%s) %s)) %s)' % (
            self.defun.name,
            self.defun.cl_args(),
            self.defun.body.cl(implicit_body=True),
            self.right.cl(implicit_body=True))


class Def(Op):
    regex = 'def'
    name = 'DEF'

    def nud(self, parser, value):
        name = parser.expression(100)
        parser.ns.push_new(return_name=name)
        parser.match('(')
        arg_names = []
        kw_args = []

        while parser.watch(')'):
            arg_name = parser.expression(10)
            if parser.maybe_match('='):
                kw_args.append((arg_name, parser.expression()))
            else:
                arg_names.append(arg_name)
            parser.maybe_match(',')
        parser.match(':')
        parser.match('NEWLINE')
        body = parser.expression()
        parser.ns.pop()
        defun = DefunNode(name, arg_names, kw_args, body)
        if parser.ns.top_level or parser.ns.class_top_level:
            return defun

        parser.ns.push_new()
        parser.ns.add(defun.name)
        flet_node = FletLambda(defun,
                               LispBody(parser.parse_rest_of_body()))
        parser.ns.pop()
        return flet_node


class ForLoopNode(LispNode):
    def __init__(self, in_node, body):
        self.in_node = in_node
        self.body = body

    def cl(self):
        collection = self.in_node.collection
        if ((collection.kind == 'call' and
             collection.name.name == 'range')):
            args = collection.args
            if len(args) == 1:
                start = 0
                step = 1
                stop = args[0]
            elif len(args) == 2:
                step = 1
                start, stop = args
            elif len(args) == 3:
                start, stop, step = args
            domain = "FROM %s BELOW %s BY %s" % (
                start, stop, step)
        else:
            domain = 'BEING THE ELEMENTS OF %s' % collection

        return '(LOOP FOR %s %s DO %s)' % (
            self.in_node.thing.cl(),
            domain,
            self.body.cl())


class For(Op):
    regex = 'for '
    name = 'FOR'

    def nud(self, parser, value):
        in_node = parser.expression(20)
        parser.ns.push_new()
        parser.ns.add(in_node.thing.name)
        parser.match(':')
        parser.match('NEWLINE')
        body = parser.expression(10)
        parser.ns.pop()
        return ForLoopNode(in_node, body)


class WhileLoopNode(LispNode):
    kind = 'while'

    def __init__(self, test, body):
        self.test = test
        self.body = body

    def cl(self):
        return '(LOOP WHILE %s DO %s)' % (
            self.test.cl(),
            self.body.cl())


class While(Op):
    regex = 'while '
    name = 'WHILE'

    def nud(self, parser, value):
        parser.ns.push_new()
        test = parser.expression(10)
        parser.match(':')
        parser.match('NEWLINE')
        body = parser.expression()
        parser.ns.pop()
        return WhileLoopNode(test, body)


class If(Op):
    lbp = 20
    regex = 'if'
    name = 'IF'


class InNode(LispNode):
    kind = 'in'

    def __init__(self, thing, collection):
        self.thing = thing
        self.collection = collection


class FindNode(InNode):
    kind = 'find'

    def cl(self):
        return '(find %s %s)' % (self.thing, self.collection)


class In(Op):
    lbp = 150
    regex = r' in '
    name = 'IN'

    def led(self, parser, left):
        return InNode(left, parser.expression())


class NoneOp(Op):
    lbp = 0
    regex = r'None'
    name = 'None'

    def nud(self, parser, value):
        return NilNode()


class ReturnNode(LispNode):
    kind = 'return'

    def __init__(self, return_expr, return_name):
        self.return_expr = return_expr
        self.return_name = return_name

    def cl(self):
        return '(RETURN-FROM %s %s)' % (self.return_name, self.return_expr)


class Return(Op):
    regex = 'return'
    name = 'RETURN'

    def nud(self, parser, value):
        return ReturnNode(parser.expression(5), parser.ns.return_name)


class SymbolNode(LispNode):
    kind = 'symbol'

    def __init__(self, name):
        self.name = name

    def cl(self):
        return '%s' % self.name


class Name(Op):
    regex = r"[^\W\d]\w*"
    name = 'NAME'

    def nud(self, parser, value):
        return SymbolNode(value)


class TupleNode(LispNode):
    kind = 'tuple'

    def __init__(self, values):
        self.values = values


class CallNode(LispNode):
    kind = 'call'

    def __init__(self, name, args, kw_args):
        self.name = name
        self.args = args
        self.kw_args = kw_args

    def cl_kw_args(self):
        forms = []
        for k, v in self.kw_args:
            forms.append(':%s %s' % (k, v))
        return ' '.join(forms)

    def cl(self):
        return '(%s %s %s)' % (self.name.cl(),
                               ' '.join(clmap(self.args)),
                               self.cl_kw_args())


class LParen(Op):
    lbp = 70
    regex = r'\('
    name = '('

    def led(self, parser, left):
        if left.kind in ('getattr', 'symbol', 'call', 'lookup', 'cl_literal'):
            name = left
            args = []
            kw_args = []
            while parser.watch(')'):
                arg = parser.expression(10)
                if parser.maybe_match('='):
                    kw_args.append((arg, parser.expression()))
                else:
                    args.append(arg)
                parser.maybe_match(',')
            if left.kind == 'getattr':
                name = left.attribute_name
                args.insert(0, left.object_name)
            return CallNode(name, args, kw_args)

    def nud(self, parser, value):
        values = []
        comma_seen = False
        while parser.watch(')'):
            values.append(parser.expression())
            if parser.maybe_match(','):
                comma_seen = True
        if comma_seen:
            return TupleNode(values)
        return values[0]


class RParen(Op):
    regex = r'\)'
    name = ')'


class ListNode(LispNode):
    kind = 'list'

    def __init__(self, values):
        self.values = values

    def cl(self):
        return ('(List)')


class GetItemNode(LispNode):
    kind = 'getitem'

    def __init__(self, left, key):
        self.left = left
        self.key = key

    def cl(self):
        return '(getitem %s %s)' % (self.left, self.key)


class LBracket(Op):
    lbp = 40
    regex = r'\['
    name = '['

    def nud(self, parser, value):
        values = []
        while parser.watch(']'):
            values.append(parser.expression(40))
            parser.maybe_match(',')
        return AdjustableArrayNode(values)

    def led(self, parser, left):
        key = parser.expression(40)
        parser.match(']')

        return GetItemNode(left, key)


class RBracket(Op):
    lbp = 40
    regex = r'\]'
    name = ']'


class LBrace(Op):
    lbp = 40
    regex = r'\{'
    name = '{'


class RBrace(Op):
    lbp = 40
    regex = r'\}'
    name = '}'


class Comma(Op):
    regex = ','
    name = ','


class EqualityNode(LispNode):
    kind = 'equal'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(EQUALP %s %s)' % (self.left.cl(), self.right.cl())


class Equality(Op):
    lbp = 40
    regex = '=='
    name = '=='

    def led(self, parser, left):
        return EqualityNode(left, parser.expression())


class IncfNode(LispNode):
    kind = 'incf'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(INCF %s %s)' % (self.left, self.right)


class IncfAssign(Op):
    lbp = 40
    regex = r'\+='
    name = '+='

    def led(self, parser, left):
        return IncfNode(left, parser.expression())


class DefParameterNode(LispNode):
    kind = 'defparameter'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(DEFPARAMETER %s %s)' % (self.left.cl(), self.right.cl())


class SetfNode(LispNode):
    kind = 'setf'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(SETF %s %s)' % (self.left.cl(), self.right.cl())


class LetNode(LispNode):
    kind = 'let'

    def __init__(self, left, right, body):
        self.pairs = [(left, right)]
        self.body = body

    def cl(self):
        return '(LET (%s) %s)' % (
            ' '.join('(%s %s)' % (l.cl(), r.cl()) for l, r in self.pairs),
            self.body.cl(implicit_body=True))


class SetItemNode(LispNode):
    kind = 'setitem'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(setitem %s %s %s)' % (
            self.left.left, self.left.key, self.right)


class Assign(Op):
    lbp = 10
    regex = '='
    name = '='

    def led(self, parser, left):
        if left.kind == 'getitem':
            return SetItemNode(left, parser.expression(10))
        elif ((left.kind == 'getattr' or
               left.name in parser.ns or
               parser.ns.cns.class_top_level)):
            return SetfNode(left, parser.expression(10))
        elif parser.ns.depth == 0:
            parser.ns.add(left.name)
            return DefParameterNode(left, parser.expression(10))
        else:
            right = parser.expression(10)
            parser.maybe_match('NEWLINE')
            parser.ns.push_new()
            parser.ns.add(left.name)
            let_node = LetNode(left,
                               right,
                               LispBody(parser.parse_rest_of_body()))
            parser.ns.pop()
            return let_node


class Colon(Op):
    regex = ':'
    name = ':'


class NumberNode(LispNode):
    kind = 'number'

    def __init__(self, value):
        self.value = value

    def cl(self):
        return '%s' % self.value


class Float(Op):
    lbp = 40
    regex = r"-?[0-9]+\.[0-9]+([eE]-?[0-9]+)?"
    name = 'FLOAT'


class Integer(Op):
    lbp = 0
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

    def cl(self):
        return '(%s %s %s)' % (self.op, self.left.cl(), self.right.cl())


class AttrLookup(LispNode):
    kind = 'getattr'

    def __init__(self, object_name, attribute_name):
        self.object_name = object_name
        self.attribute_name = attribute_name

    def cl(self):
        return "(SLOT-VALUE %s '%s)" % (self.object_name.cl(),
                                        self.attribute_name.cl())


class Dot(Op):
    lbp = 150
    regex = r'\.'
    name = 'DOT'

    def led(self, parser, left):
        right = parser.expression(150)
        if right.kind == 'call':
            right.args.insert(0, left)
            return right
        return AttrLookup(left, right)


class LessThan(Op):
    lbp = 130
    regex = '<'
    name = 'LESSTHAN'

    def led(self, parser, left):
        return BinaryOperatorNode('<', left, parser.expression())


class Plus(Op):
    lbp = 50
    regex = r'\+'
    name = 'PLUS'

    def led(self, parser, left):
        return BinaryOperatorNode('+', left, parser.expression(50))


class Minus(Op):
    lbp = 50
    regex = r'\-'
    name = 'MINUS'

    def led(self, parser, left):
        return BinaryOperatorNode('-', left, parser.expression(110))


class SplatNode(LispNode):
    kind = 'splat'

    def __init__(self, right):
        self.right = right


class Multiply(Op):
    lbp = 60
    regex = r'\*'
    name = 'MULTIPLY'

    def led(self, parser, left):
        return BinaryOperatorNode('*', left, parser.expression(110))

    def nud(self, parser, value):
        return SplatNode(parser.expression())


class Divide(Op):
    lbp = 60
    regex = r'\/'
    name = 'DIVIDE'

    def led(self, parser, left):
        return BinaryOperatorNode('/', left, parser.expression(110))


class StringNode(LispNode):
    kind = 'string'

    def __init__(self, value):
        self.value = value

    def cl(self):
        return '"%s"' % self.value


class String(Op):
    regexes = [#r"'''(.*?)'''",
               #r"'''(.*?)'''",
               r"'(.*?)'",
               r'"(.*?)"']
    name = 'STRING'

    def nud(self, parser, value):
        value = value[1:-1]

        return StringNode(value)


class LispLiteralNode(LispNode):
    kind = 'cl_literal'

    def __init__(self, literal):
        self.literal = literal
        self.name = literal

    def cl(self):
        return self.literal


class LispLiteral(Op):
    regex = '`(.*?)`'
    name = 'cl_literal'

    def nud(self, parser, value):
        value = value[1:-1]
        return LispLiteralNode(value)


class Newline(Op):
    regex = r'\n'
    name = 'NEWLINE'


class Whitespace(Op):
    regex = r'[ ]+'
    name = 'WHITESPACE'
