# -*- coding: utf-8 -*-
from __future__ import unicode_literals

all_ops = []


lisp_prefix = """
(cl:require :asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
"""

lisp_postfix = """
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))
"""


def find_self_assignments(n):
    assignments = []
    if n.kind == 'body':
        for form in n.forms:
            assignments.extend(find_self_assignments(form))
    elif n.kind in ('defun', 'condclause', 'while', 'for'):
        assignments.extend(find_self_assignments(n.body))
    elif n.kind == 'setf':
        assignments.extend(find_self_assignments(n.left))
    elif n.kind == 'cond':
        for clause in n.clauses:
            assignments.extend(find_self_assignments(clause))
    elif n.kind == 'getattr':
        if n.object_name.name == 'self':
            assignments.append(n.attribute_name.name)

    return assignments


def unbox_arglist(t):
    args = []
    kwargs = []
    if t.kind != 'tuple':
        t = Tuple([t])
    for arg in t.values:
        if arg.kind == 'setf':
            kwargs.append((arg.left, arg.right))
        else:
            args.append(arg)

    return args, kwargs


def register(cls):
    all_ops.append(cls)
    return cls


class Lisp(object):
    kind = 'node'

    def __repr__(self):
        return self.cl()

    def clmap(self, forms):
        return ['%s' % x for x in forms]

    def cl(self):
        return '(%s-XXX XXX)' % self.kind


class Comment(Lisp):
    kind = 'comment'

    def __init__(self, comment):
        self.comment = comment

    def cl(self):
        return '#|%s|#' % self.comment


class LispBody(Lisp):
    kind = 'body'

    def __init__(self, forms):
        self.forms = forms

    def cl(self, implicit_body=False):
        if implicit_body:
            return '\n'.join(self.clmap(self.forms))
        elif len(self.forms) > 1 and not implicit_body:
            return '(PROGN %s)' % '\n'.join(self.clmap(self.forms))
        elif len(self.forms) == 1:
            return self.forms[0].cl()
        else:
            # Do we need this?
            return 'nil'


class MakePackage(Lisp):
    def __init__(self, name):
        self.name = name

    def cl(self):
        return '(make-package "%s" :use \'("COMMON-LISP"))' % self.name


class LispPackage(Lisp):
    kind = 'package'

    def __init__(self, module_name, block):
        self.module_name = module_name
        self.block = block

    def cl(self):
        forms = []
        forms.append('(eval-when (:compile-toplevel :load-toplevel :execute)')
        forms.append('(unless (find-package "%s")' % self.module_name)
        forms.append(MakePackage(self.module_name).cl())
        forms.append(') ')
        forms.append(') ')
        forms.append('(in-package "%s")' % self.module_name)
        forms.append(lisp_prefix)
        forms.append('(use-package "builtins")')
        forms.append(self.block.cl(implicit_body=True))
        forms.append(lisp_postfix)
        return ''.join(forms)


class Method(Lisp):
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


class CLOSClass(Lisp):
    kind = 'class'

    def __init__(self, name, bases=(), slots=(), members=(), methods=()):
        self.name = name
        self.bases = list(bases)
        self.slots = list(slots)
        self.members = list(members)
        self.methods = list(methods)
        self.constructor = None

    def add_form(self, form):
        if form.kind == 'defun':
            if form.name.name == '__init__':
                self.constructor = form
                self.slots = find_self_assignments(form)
            else:
                self.methods.append(form)

    def cl_method(self, defun):
        return Method(self.name, defun)

    def cl_methods(self):
        return ' '.join(self.cl_method(defun).cl() for defun in self.methods)

    def cl_bases(self):
        return '(%s)' % ' '.join(base.cl() for base in self.bases)

    def cl_slot(self, slot):
        return '|%s|' % slot

    def cl_slots(self):
        if self.slots:
            return '(%s)' % ' '.join(self.cl_slot(slot) for slot in self.slots)
        else:
            return 'NIL'

    def cl_init_call(self):
        if not self.constructor:
            return ''
        return '(|init| |self| %s)' % self.cl_init_args()

    def cl_init_args(self):
        if self.constructor is None:
            return ''
        return self.constructor.cl_args(skip_first=True)

    def cl_constructor(self):
        if self.constructor:
            body_forms = self.constructor.body.forms
            arg_names = self.constructor.arg_names[1:]
            kw_args = self.constructor.kw_args
        else:
            body_forms = []
            arg_names = []
            kw_args = []
        forms = list(body_forms) + [Symbol('self')]
        body = Let(
            Symbol('self'),
            LispLiteral("(make-instance '%s)" % self.name),
            LispBody(forms))

        defun = Defun(
            self.name,
            arg_names,
            kw_args,
            LispBody([body]))

        return "%s" % defun

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


class Defun(Lisp):
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


class FletLambda(Defun):
    def __init__(self, defun, right):
        self.defun = defun
        self.right = right

    def cl(self):
        return '(FLET ((%s (%s) %s)) %s)' % (
            self.defun.name,
            self.defun.cl_args(),
            self.defun.body.cl(implicit_body=True),
            self.right.cl(implicit_body=True))


class ForLoop(Lisp):
    kind = 'for'

    def __init__(self, in_node, body):
        self.in_node = in_node
        self.body = body

    def cl(self):
        collection = self.in_node.collection
        if ((collection.kind == 'call' and
             collection.name.name in ('range', 'xrange'))):
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

        if self.in_node.thing.kind == 'type':
            var = self.in_node.thing.of_type_cl()
        else:
            var = self.in_node.thing.cl()
        return '(LOOP FOR %s %s DO %s)' % (
            var,
            domain,
            self.body.cl())


class CondClause(Lisp):
    kind = 'condclause'

    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def cl(self):
        return '(%s %s)' % (self.condition, self.body)


class Cond(Lisp):
    kind = 'cond'

    def __init__(self, clauses):
        self.clauses = clauses

    def cl(self):
        return '(COND %s)' % ' '.join('%s' % c for c in self.clauses)


class Return(Lisp):
    kind = 'return'

    def __init__(self, return_expr, return_name):
        self.return_expr = return_expr
        self.return_name = return_name

    def cl(self):
        return '(RETURN-FROM %s %s)' % (self.return_name, self.return_expr)


class Symbol(Lisp):
    kind = 'symbol'

    def __init__(self, name, namespace=None):
        self.name = name
        self.namespace = namespace

    def cl(self):
        if self.namespace is not None:
            return '|%s|:|%s|' % (self.namespace, self.symbol)
        return '|%s|' % self.name


class WhileLoop(Lisp):
    kind = 'while'

    def __init__(self, test, body):
        self.test = test
        self.body = body

    def cl(self):
        return '(LOOP WHILE %s DO %s)' % (
            self.test.cl(),
            self.body.cl())


class In(Lisp):
    kind = 'in'

    def __init__(self, thing, collection):
        self.thing = thing
        self.collection = collection


class Find(In):
    kind = 'find'

    def cl(self):
        return '(find %s %s)' % (self.thing, self.collection)


class Nil(Lisp):
    kind = 'nil'

    def cl(self):
        return 'NIL'


class UsePackage(Lisp):
    kind = 'use'

    def __init__(self, right):
        self.right = right

    def cl(self):
        return '(use-package "%s")' % self.right.name


class List(Lisp):
    kind = 'list'

    def __init__(self, values):
        self.values = values

    def cl(self):
        return ("(|list| '(%s))" % ' '.join(self.clmap(self.values)))


class GetItem(Lisp):
    kind = 'getitem'

    def __init__(self, left, key):
        self.left = left
        self.key = key

    def cl(self):
        return '(|getitem| %s %s)' % (self.left, self.key)


class Tuple(Lisp):
    kind = 'tuple'

    def __init__(self, values):
        self.values = values

    def cl(self):
        return "(|tuple| '(%s))" % ' '.join(self.clmap(self.values))


class Call(Lisp):
    kind = 'call'

    def __init__(self, name, args, kw_args=()):
        self.name = name
        self.args = args
        self.kw_args = kw_args

    def cl_kw_args(self):
        forms = []
        for k, v in self.kw_args:
            forms.append(':%s %s' % (k, v))
        return ' '.join(forms)

    def cl(self):
        return '(%s %s %s)' % (self.name,
                               ' '.join(self.clmap(self.args)),
                               self.cl_kw_args())


class Type(Lisp):
    kind = 'type'

    def __init__(self, type, left):
        self.type = type
        self.left = left

    @property
    def name(self):
        return self.left.name

    def cl(self):
        return '(the %s %s)' % (self.type, self.left)

    def of_type_cl(self):
        return '%s of-type %s' % (self.left, self.type)


class Equality(Lisp):
    kind = 'equal'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(EQUALP %s %s)' % (self.left.cl(), self.right.cl())


class DefParameter(Lisp):
    kind = 'defparameter'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(DEFPARAMETER %s %s)' % (self.left.cl(), self.right.cl())


class Setf(Lisp):
    kind = 'setf'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(SETF %s %s)' % (self.left, self.right)


class Let(Lisp):
    kind = 'let'

    def __init__(self, left, right, body):
        self.pairs = [(left, right)]
        self.body = body

    def cl(self):
        declares = []
        pairs = []
        for l, r in self.pairs:
            if l.kind == 'type':
                declares.append(Call('DECLARE',
                                     [Call('TYPE', [l.type, l.left])]))
                pairs.append((l.left, r))
            else:
                pairs.append((l, r))
        for declare in declares:
            self.body.forms.insert(0, declare)

        return '(LET (%s) %s)' % (
            ' '.join('(%s %s)' % (l, r) for l, r in pairs),
            self.body.cl(implicit_body=True))


class SetItem(Lisp):
    kind = 'setitem'

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def cl(self):
        return '(|setitem| %s %s %s)' % (
            self.left.left, self.left.key, self.right)


class Number(Lisp):
    kind = 'number'

    def __init__(self, value):
        self.value = value

    def cl(self):
        return '%s' % self.value


class BinaryOperator(Lisp):
    kind = 'binary_op'

    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def cl(self):
        return '(%s %s %s)' % (self.op, self.left.cl(), self.right.cl())


class AttrLookup(Lisp):
    kind = 'getattr'

    def __init__(self, object_name, attribute_name):
        self.object_name = object_name
        self.attribute_name = attribute_name

    def cl(self):
        return "(SLOT-VALUE %s '%s)" % (self.object_name,
                                        self.attribute_name)


class Splat(Lisp):
    kind = 'splat'

    def __init__(self, right):
        self.right = right


class String(Lisp):
    kind = 'string'

    def __init__(self, value):
        self.value = value

    def cl(self):
        return '"%s"' % self.value


class LispLiteral(Lisp):
    kind = 'cl_literal'

    def __init__(self, literal):
        self.literal = literal
        self.name = literal

    def cl(self):
        return self.literal


class Token(object):
    name = None
    lbp = 0
    start_chars = set()
    rest_chars = set()

    def __init__(self, value='', line=None, column=None):
        self.value = value
        self.line = line
        self.column = column

    def spawn(self, token_class=None, name=None, lbp=None):
        if token_class is None:
            token_class = Token
        token = token_class(self.value)
        if name is not None:
            token.name = name
        if lbp is not None:
            token.lbp = lbp
        return token

    @classmethod
    def can_start(self, c):
        return c in self.start_chars

    def match(self, c):
        if c in self.rest_chars:
            return True
        return False

    def complete(self):
        return True

    def handle(self, c):
        self.value += c
        return self

    def __repr__(self):
        return '( %r :: %s )' % (self.value, self.name)


class EnumeratedToken(Token):
    lbp_map = {}

    @classmethod
    def can_start(self, c):
        for symbol in self.lbp_map:
            if symbol.startswith(c):
                return True
        return False

    def match(self, c):
        for symbol in self.lbp_map:
            if symbol.startswith(self.value + c):
                return True
        return False

    def handle(self, c):
        self.value += c
        return self

    def complete(self):
        if self.value in self.lbp_map:
            self.name = self.value
            self.lbp = self.lbp_map[self.value]
            return True
        return False


@register
class NoDispatchTokens(EnumeratedToken):
    lbp_map = {
        ')': 0,
        ']': 0,
        '}': 0}


@register
class BinOpToken(EnumeratedToken):
    lbp_map = {
        '%': 0,
        '&': 0,
        '*': 60,
        '**': 0,
        '+': 50,
        '-': 50,
        '/': 60,
        '//': 60,
        '<': 130,
        '<<': 0,
        '>': 130,
        '>>': 0,
        '^': 0,
        '|': 0,
        '~': 0}

    def led(self, parser, left):
        return BinaryOperator(self.value,
                              left,
                              parser.expression(self.lbp_map[self.value]))

    def nud(self, parser, value):
        if value == '*':
            return Splat(parser.expression())
        if value == '-':
            return Call('-', [parser.expression()])


@register
class AugAssign(EnumeratedToken):
    lbp_map = {
        '!=': 0,
        '%=': 0,
        '&=': 0,
        '*=': 0,
        '**=': 0,
        '+=': 0,
        '-=': 0,
        '//=': 0,
        '<<=': 0,
        '<=': 0,
        '>>=': 0,
        '>=': 0,
        '/=': 0,
        '^=': 0,
        '|=': 0}


@register
class Colon(EnumeratedToken):
    lbp_map = {':': 0,
               '::': 200}

    def led(self, parser, left):
        right = parser.expression(200)
        return Type(right, left)


@register
class AssignOrEquals(EnumeratedToken):
    lbp_map = {
        '==': 40,
        '=': 10}

    def led(self, parser, left):
        if self.value == '=':
            if left.kind == 'getitem':
                return SetItem(left, parser.expression(10))
            elif ((left.kind == 'getattr' or
                   parser.ns.inside_form or
                   parser.ns.class_top_level or
                   left.name in parser.ns)):
                return Setf(left, parser.expression(10))
            elif parser.ns.depth == 0:
                parser.ns.add(left.name)
                return DefParameter(left, parser.expression(10))
            else:
                right = parser.expression(10)
                parser.maybe_match('NEWLINE')
                parser.ns.push_new()
                parser.ns.add(left.name)
                let_node = Let(left,
                               right,
                               LispBody(parser.parse_rest_of_body()))
                parser.ns.pop()
                return let_node
        else:
            return Equality(left, parser.expression())


@register
class Module(Token):
    name = 'MODULE'

    def nud(self, parser, value):
        parser.ns.push_new()
        package = LispPackage(parser.filename, parser.expression())
        parser.ns.pop()
        return package


@register
class Block(Token):
    lbp = 0
    name = 'BLOCK'

    def nud(self, parser, value):
        forms = parser.parse_rest_of_body()
        parser.match('ENDBLOCK')
        return LispBody(forms)


@register
class Endblock(Token):
    lbp = 0
    name = 'ENDBLOCK'


@register
class Name(Token):
    lbp = 0
    name = 'NAME'
    start_chars = set('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_')
    rest_chars = start_chars | set('0123456789')

    def complete(self):
        if self.value == 'in':
            self.lbp = 150
        elif self.value == 'is':
            self.lbp = 140
        elif self.value == 'elif':
            self.name = 'ELIF'
        elif self.value == 'else':
            self.name = 'ELSE'
        return True

    def nud(self, parser, value):
        if value == 'load':
            return ''
        if value == 'assert':
            return Call('ASSERT', [parser.expression()])
        elif value == 'True':
            return LispLiteral('t')
        elif value == 'if':
            cond_clauses = []
            parser.ns.push_new()
            condition = parser.expression(10)
            parser.match(':')
            parser.match('NEWLINE')
            body = parser.expression()
            parser.ns.pop()
            cond_clauses.append(CondClause(condition, body))
            while parser.maybe_match('ELIF'):
                parser.ns.push_new()
                condition = parser.expression(10)
                parser.match(':')
                parser.match('NEWLINE')
                body = parser.expression()
                parser.ns.pop()
                cond_clauses.append(CondClause(condition, body))
            if parser.maybe_match('ELSE'):
                parser.ns.push_new()
                condition = LispLiteral('t')
                parser.match(':')
                parser.match('NEWLINE')
                body = parser.expression()
                parser.ns.pop()
                cond_clauses.append(CondClause(condition, body))
            return Cond(cond_clauses)
        elif value == 'pass':
            return Nil()
        elif value == 'while':
            parser.ns.push_new()
            test = parser.expression(10)
            parser.match(':')
            parser.match('NEWLINE')
            body = parser.expression()
            parser.ns.pop()
            return WhileLoop(test, body)
        elif value == 'None':
            return Nil()
        elif value == 'return':
            return Return(parser.expression(5), parser.ns.return_name)
        elif value == 'class':
            name = parser.expression(80)
            cc = CLOSClass(name)
            if parser.maybe_match('('):
                while parser.watch(')'):
                    cc.bases.append(parser.expression(40))
                    parser.maybe_match(',')
            parser.match(':')
            parser.match('NEWLINE')
            parser.ns.push_new(class_top_level=True)
            body = parser.expression()
            parser.ns.pop()
            for form in body.forms:
                cc.add_form(form)
            return cc
        elif value == 'def':
            name = parser.expression(100)
            parser.ns.push_new(return_name=name)
            parser.match('(')
            parser.maybe_match('NEWLINE')
            arg_names = []
            kw_args = []

            while parser.watch(')'):
                arg_name = parser.expression(40)
                if parser.maybe_match('='):
                    kw_args.append((arg_name, parser.expression(40)))
                else:
                    arg_names.append(arg_name)
                parser.maybe_match('NEWLINE')
                parser.maybe_match(',')
                parser.maybe_match('NEWLINE')
            parser.match(':')
            parser.match('NEWLINE')
            body = parser.expression()
            parser.ns.pop()
            defun = Defun(name, arg_names, kw_args, body)
            if parser.ns.top_level or parser.ns.class_top_level:
                return defun

            parser.ns.push_new()
            parser.ns.add(defun.name)
            flet_node = FletLambda(
                defun, LispBody(parser.parse_rest_of_body()))
            parser.ns.pop()
            return flet_node
        elif value == 'for':
            in_node = parser.expression(20)
            parser.ns.push_new()
            parser.ns.add(in_node.thing.name)
            parser.match(':')
            parser.match('NEWLINE')
            body = parser.expression(10)
            parser.ns.pop()
            return ForLoop(in_node, body)
        elif value == 'use':
            right = parser.expression(5)
            return UsePackage(right)
        else:
            if value == value.upper():
                value = value.replace('_', '-')
            return Symbol(value)

    def led(self, parser, left):
        if self.value == 'in':
            return In(left, parser.expression())
        elif self.value == 'is':
            return BinaryOperator('eq', left, parser.expression())


@register
class LParen(Token):
    name = '('
    lbp = 70
    start_chars = {'('}

    def led(self, parser, left):
        if left.kind in ('getattr', 'symbol', 'call', 'lookup', 'cl_literal'):
            parser.ns.push_new(inside_form=True)
            name = left
            args = []
            kw_args = []
            while parser.watch(')'):
                arg = parser.expression(40)
                if parser.maybe_match('='):
                    kw_args.append((arg, parser.expression(40)))
                else:
                    args.append(arg)
                parser.maybe_match('NEWLINE')
                parser.maybe_match(',')
                parser.maybe_match('NEWLINE')
            if left.kind == 'getattr':
                name = left.attribute_name
                args.insert(0, left.object_name)
            parser.ns.pop()
            return Call(name, args, kw_args)

    def nud(self, parser, value):
        values = []
        comma_seen = False
        while parser.watch(')'):
            values.append(parser.expression())
            if parser.maybe_match(','):
                comma_seen = True
        if comma_seen or not values:
            return Tuple(values)
        return values[0]


@register
class LBracket(Token):
    lbp = 40
    start_chars = {'['}
    name = '['

    def nud(self, parser, value):
        values = []
        while parser.watch(']'):
            values.append(parser.expression(40))
            parser.maybe_match(',')
        return List(values)

    def led(self, parser, left):
        key = parser.expression(40)
        parser.match(']')

        return GetItem(left, key)


@register
class LBrace(Token):
    lbp = 40
    start_chars = {'{'}
    name = '{'

    def nud(self, parser, value):
        key_vals = []
        while parser.watch('}'):
            key = parser.expression()
            parser.match(':')
            val = parser.expression()
            key_vals.append(key, val)
        parser.log('%s', key_vals)


@register
class NumberToken(Token):
    start_chars = set('0123456789')
    rest_chars = start_chars | set('ex')
    name = 'NUMBER'

    def nud(self, parser, value):
        return Number(value)


@register
class Dot(Token):
    lbp = 150
    start_chars = {'.'}
    name = 'DOT'

    def led(self, parser, left):
        right = parser.expression(150)
        if right.kind == 'call':
            right.args.insert(0, left)
            return right
        return AttrLookup(left, right)


class EscapingToken(Token):
    def __init__(self, c='', line=None, column=None):
        super(EscapingToken, self).__init__(c, line, column)


@register
class StringToken(EscapingToken):
    start_chars = {'"', "'"}
    name = 'STRING'

    @property
    def multiline(self):
        if len(self.value) < 3:
            return True
        elif self.value.startswith(self.value[0] * 3):
            return True
        return False

    def match(self, c):
        if self.multiline:
            min_len, escape_pos, slice_size = (6, -4, 3)
        else:
            min_len, escape_pos, slice_size = (2, -2, 1)
        if len(self.value) < min_len:
            return True
        elif self.value[escape_pos] == '\\':
            return True
        elif self.value[:slice_size] != self.value[-slice_size:]:
            return True
        else:
            return False

    def nud(self, parser, value):
        value = value[1:-1]

        return String(value)


@register
class Backslash(Token):
    name = '\\'
    start_chars = {'\\'}

    def nud(self, parser, value):
        return parser.expression()


@register
class Backtick(StringToken):
    name = '`'
    start_chars = {'`'}

    def nud(self, parser, value):
        value = value[1:-1]
        return LispLiteral(value)


@register
class Newline(Token):
    name = 'NEWLINE'
    start_chars = {'\n'}

    def nud(self, parser, value):
        return parser.expression()


@register
class Comma(Token):
    name = ','
    start_chars = ','
    lbp = 30

    def led(self, parser, left):
        values = [left, parser.expression(30)]
        while parser.maybe_match(','):
            values.append(parser.expression(30))

        return Tuple(values)


@register
class Whitespace(Token):
    name = 'WHITESPACE'
    start_chars = {' '}
    rest_chars = start_chars


@register
class CommentToken(EscapingToken):
    start_chars = {'#'}

    def match(self, c):
        if self.value[-1] == '\n':
            return False
        return True

    def nud(self, parser, value):
        return Comment(value[1:])
