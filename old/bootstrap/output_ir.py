import ast as _ast
from sys import stdout
from sys import stderr

import llvmlite.ir as ll

from namespace import LinkedNamespace


def classname(inst):
    return inst.__class__.__name__


class Compiler:
    lexical = False

    def __init__(self, *, source=None, ast=None, debug=False, depth=0, ns=None):
        self.debug = debug
        self.depth = depth

        if ns is None:
            self.ns = LinkedNamespace()
        else:
            self.ns = ns

        if source is not None:
            self.source = source
            self.ast = _ast.parse(source)
        elif ast is not None:
            self.source = None
            self.ast = ast
        else:
            raise ValueError('You must specify source code or an ast object')

    def log(self, msg):
        print('{}{}'.format('    ' * self.depth, msg), file=stderr)

    def compile(self):
        if self.debug:
            self.log(f'{classname(self)}.compile()')
        return self._compile()

    def _compile(self):
        yield f' <<{classname(self)}.TODO>> '

    def make_node(self, ast):
        cls = globals()[classname(ast)]
        if cls.lexical:
            ns = self.ns.make_child()
        else:
            ns = self.ns
        return cls(ast=ast, ns=ns, debug=self.debug, depth=self.depth + 1)

    def write_ir(self, fn_or_f):
        if hasattr(fn_or_f, 'write'):
            self._write_ir(fn_or_f)
        else:
            with open(fn_or_f, 'w') as f:
                self._write_ir(f)

    def _write_ir(self, f):
        self.layout_nodes()
        for ir in self.compile():
            f.write(ir)

    def layout_nodes(self):
        self.map_all_fields()

    def map_field(self, field_name, *, recurse=True):
        if self.debug:
            self.log(f'{classname(self)}.map_field({field_name!r})')

        field_value = getattr(self.ast, field_name)

        if field_value is None:
            setattr(self, field_name, None)

        elif type(field_value) == list:
            nodes = []
            setattr(self, field_name, nodes)
            for entry in field_value:
                node = self.make_node(entry)
                if recurse:
                    node.layout_nodes()
                nodes.append(node)

        else:
            node = self.make_node(field_value)
            if recurse:
                node.layout_nodes()
            setattr(self, field_name, node)

    def map_all_fields(self, *, recurse=True):
        for field_name in self.ast._fields:
            self.map_field(field_name, recurse=recurse)


class Expression(Compiler):
    def __init__(self, *args, **kw):
        super(Expression, self).__init__(*args, **kw)
        self.type = 'NoneType'


class Module(Compiler):
    'Module(stmt* body)'
    def _init(self):
        self.module = ll.Module()

    def layout_nodes(self):
        pass

    def _compile(self):
        for stmt in self.body:
            yield from stmt.compile()


class Interactive(Compiler):
    'Interactive(stmt* body)'


class FunctionDef(Compiler):
    '''FunctionDef(identifier name, arguments args, stmt* body,
                   expr* decorator_list, expr? returns)'''

    lexical = True

    def layout_nodes(self):
        self.name = self.ast.name
        self.map_field('args')
        self.map_field('body')
        self.map_field('decorator_list')
        self.map_field('returns')

    def _compile(self):
        yield 'define '
        if self.returns is not None:
            yield self.returns
            yield ' '
        else:
            yield 'void '

        yield '@'
        yield self.name
        yield '('
        yield from self.args.compile()
        yield ') {\n'

        for node in self.body:
            yield from node.compile()

        yield '\n}\n\n'


class AsyncFunctionDef(Compiler):
    '''AsyncFunctionDef(identifier name, arguments args, stmt* body,
                        expr* decorator_list, expr? returns)'''


class ClassDef(Compiler):
    lexical = True

    '''ClassDef(identifier name, expr* bases, keyword* keywords,
                stmt* body, expr* decorator_list)'''


class Return(Compiler):
    'Return(expr? value)'

    def _compile(self):
        if self.value is None:
            yield 'ret NoneType NoneType'
            return
        yield 'ret '
        yield self.value.type
        yield ' '

        yield from self.value.compile()


class Delete(Compiler):
    'Delete(expr* targets)'


class Assign(Compiler):
    'Assign(expr* targets, expr value)'

    def _compile(self):
        yield self.targets[0].id
        yield ' = '
        yield from self.value.compile()


class AugAssign(Compiler):
    'AugAssign(expr target, operator op, expr value)'


class AnnAssign(Compiler):
    'AnnAssign(expr target, expr annotation, expr? value, int simple)'


class For(Compiler):
    'For(expr target, expr iter, stmt* body, stmt* orelse)'


class AsyncFor(Compiler):
    'AsyncFor(expr target, expr iter, stmt* body, stmt* orelse)'


class While(Compiler):
    'While(expr test, stmt* body, stmt* orelse)'


class If(Compiler):
    'If(expr test, stmt* body, stmt* orelse)'

    def _compile(self):
        yield '%blahblah = '
        yield from self.test.compile()
        yield '\n'

        yield 'br i1 %cond, label %ifyes, label %ifno\n'

        yield '\n'
        yield '%ifyes:\n'
        for stmt in self.body:
            yield from stmt.compile()

        yield '\n'
        yield '%ifno:\n'
        for el in self.orelse:
            yield from el.compile()


class With(Compiler):
    'With(withitem* items, stmt* body)'


class AsyncWith(Compiler):
    'AsyncWith(withitem* items, stmt* body)'


class Raise(Compiler):
    'Raise(expr? exc, expr? cause)'


class Try(Compiler):
    'Try(stmt* body, excepthandler* handlers, stmt* orelse, stmt* finalbody)'


class Assert(Compiler):
    'Assert(expr test, expr? msg)'

    # def _compile(self):
    #     yield '<<'


class Import(Compiler):
    'Import(alias* names)'


class ImportFrom(Compiler):
    'ImportFrom(identifier? module, alias* names, int? level)'


class Global(Compiler):
    'Global(identifier* names)'


class Nonlocal(Compiler):
    'Nonlocal(identifier* names)'


class Expr(Expression):
    'Expr(expr value)'

    def _compile(self):
        yield from self.value.compile()


class Pass(Compiler):
    pass


class Break(Compiler):
    pass


class Continue(Compiler):
    pass


class attributes (Compiler):
    'attributes (int lineno, int col_offset)'


class BoolOp(Compiler):
    'BoolOp(boolop op, expr* values)'


class BinOp(Expression):
    'BinOp(expr left, operator op, expr right)'
    def __str__(self):
        return f'<<BinOp({self.left} {self.op} {self.right})>>'

    def _compile(self):
        yield str(self)


class UnaryOp(Compiler):
    'UnaryOp(unaryop op, expr operand)'


class Lambda(Compiler):
    'Lambda(arguments args, expr body)'


class IfExp(Compiler):
    'IfExp(expr test, expr body, expr orelse)'


class Dict(Compiler):
    'Dict(expr* keys, expr* values)'


class Set(Compiler):
    'Set(expr* elts)'


class ListComp(Compiler):
    'ListComp(expr elt, comprehension* generators)'


class SetComp(Compiler):
    'SetComp(expr elt, comprehension* generators)'


class DictComp(Compiler):
    'DictComp(expr key, expr value, comprehension* generators)'


class GeneratorExp(Compiler):
    'GeneratorExp(expr elt, comprehension* generators)'


class Await(Compiler):
    'Await(expr value)'


class Yield(Compiler):
    'Yield(expr? value)'


class YieldFrom(Compiler):
    'YieldFrom(expr value)'


class Compare(Compiler):
    'Compare(expr left, cmpop* ops, expr* comparators)'


class Call(Expression):
    'Call(expr func, expr* args, keyword* keywords)'
    def _compile(self):
        yield self.func.id
        yield '('
        cnt = 0
        for arg in self.args:
            if cnt:
                yield ', '
            yield from arg.compile()
            cnt += 1

        cnt = 0
        for keyword in self.keywords:
            if cnt:
                yield ', '
            yield from keyword.compile()
            cnt += 1

        yield ')\n'


class Num(Compiler):
    'Num(object n) -- a number as a PyObject.'
    def layout_nodes(self):
        self.n = self.ast.n

    def _compile(self):
        yield str(self.n)


class Str(Compiler):
    'Str(string s) -- need to specify raw, unicode, etc?'
    def layout_nodes(self):
        self.s = self.ast.s


class FormattedValue(Compiler):
    'FormattedValue(expr value, int? conversion, expr? format_spec)'


class JoinedStr(Compiler):
    'JoinedStr(expr* values)'


class Bytes(Compiler):
    'Bytes(bytes s)'


class NameConstant(Compiler):
    'NameConstant(singleton value)'


class Ellipsis(Compiler):
    pass


class Constant(Compiler):
    'Constant(constant value)'


class Attribute(Compiler):
    'Attribute(expr value, identifier attr, expr_context ctx)'


class Subscript(Compiler):
    'Subscript(expr value, slice slice, expr_context ctx)'


class Starred(Compiler):
    'Starred(expr value, expr_context ctx)'


class Name(Expression):
    'Name(identifier id, expr_context ctx)'
    def layout_nodes(self):
        self.id = self.ast.id
        self.map_field('ctx')

    def _compile(self):
        yield '%'
        yield self.id


class List(Compiler):
    'List(expr* elts, expr_context ctx)'


class Tuple(Compiler):
    'Tuple(expr* elts, expr_context ctx)'


class Store(Compiler):
    pass


class Load(Compiler):
    pass


class arguments(Compiler):
    '''arguments = (arg* args, arg? vararg, arg* kwonlyargs, expr* kw_defaults,
                    arg? kwarg, expr* defaults)'''

    def layout_nodes(self):
        self.map_field('args')
        self.map_field('vararg')
        self.map_field('kwonlyargs')
        self.map_field('kw_defaults')
        self.map_field('kwarg')
        self.map_field('defaults')

    def _compile(self):
        cnt = 0
        for arg in self.args:
            if cnt:
                yield ', '
            yield from arg.compile()
            cnt += 1


class Add(Compiler):
    pass


class Sub(Compiler):
    pass


class Mult(Compiler):
    pass


class MatMult(Compiler):
    pass


class Div(Compiler):
    pass


class Mod(Compiler):
    pass


class Pow(Compiler):
    pass


class LShift(Compiler):
    pass


class RShift(Compiler):
    pass


class BitOr(Compiler):
    pass


class BitXor(Compiler):
    pass


class BitAnd(Compiler):
    pass


class FloorDiv(Compiler):
    pass


class Slice(Compiler):
    'Slice(expr? lower, expr? upper, expr? step)'


class ExtSlice(Compiler):
    'ExtSlice(slice* dims)'


class Index(Compiler):
    'Index(expr value)'


class Eq(Compiler):
    pass


class NotEq(Compiler):
    pass


class Lt(Compiler):
    pass


class LtE(Compiler):
    pass


class Gt(Compiler):
    pass


class GtE(Compiler):
    pass


class Is(Compiler):
    pass


class IsNot(Compiler):
    pass


class In(Compiler):
    pass


class NotIn (Compiler):
    pass


class keyword(Compiler):
    'keyword = (identifier? arg, expr value)'
    def layout_nodes(self):
        self.id = self.ast.arg
        self.map_field('value')

    def _compile(self):
        yield self.id
        yield '='
        yield str(self.value.n)


class arg(Compiler):
    '''arg = (identifier arg, expr? annotation)'''

    def layout_nodes(self):
        self.id = self.ast.arg
        self.map_field('annotation')

    def _compile(self):
        yield self.id


'''
    expr_context = Load  Store  Del  AugLoad  AugStore  Param

    slice =

    boolop = And  Or

    operator = Add  Sub  Mult  MatMult  Div  Mod  Pow  LShift
                  RShift  BitOr  BitXor  BitAnd  FloorDiv

    unaryop = Invert  Not  UAdd  USub

    cmpop = Eq  NotEq  Lt  LtE  Gt  GtE  Is  IsNot  In  NotIn

    comprehension = (expr target, expr iter, expr* ifs, int is_async)

    excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
                    attributes (int lineno, int col_offset)

    arguments = (arg* args, arg? vararg, arg* kwonlyargs, expr* kw_defaults,
                 arg? kwarg, expr* defaults)

    arg = (identifier arg, expr? annotation)
    attributes (int lineno, int col_offset)

    -- keyword arguments supplied to call (NULL identifier for **kwargs)

    -- import name with optional 'as' alias.
    alias = (identifier name, identifier? asname)

    withitem = (expr context_expr, expr? optional_vars)'''


def main():
    import argparse
    parser = argparse.ArgumentParser(description='Run training')
    parser.add_argument('source_fn',
                        help="fn")
    parser.add_argument('output_fn',
                        nargs='?',
                        help="fn or - for stdout")
    parser.add_argument('-d', '--debug',
                        action='store_true',
                        help="debug output")
    args = parser.parse_args()

    with open(args.source_fn) as f:
        source = f.read()
        module = Module(source=source, debug=args.debug)
        module.write_ir(stdout)



if __name__ == '__main__':
    main()
