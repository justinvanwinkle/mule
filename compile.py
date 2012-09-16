from __future__ import print_function
from __future__ import unicode_literals

from ast import parse
from ast import iter_child_nodes
from ast import NodeVisitor
from io import StringIO


class Namespace(object):
    def __init__(self):
        self.s = set()
        self.let_count = 0

    def add(self, name):
        self.s.add(name)

    def __contains__(self, name):
        return name in self.s


class NamespaceStack(object):
    def __init__(self):
        self.stack = []
        self.cns = None

    def push_new_namespace(self):
        ns = Namespace()
        self.cns = ns
        self.stack.append(ns)

    def pop_namespace(self):
        lost_namespace = self.stack[-1]
        self.stack = self.stack[:-1]
        self.cns = self.stack[-1]
        return lost_namespace

    def add(self, name):
        self.cns.add(name)

    def __contains__(self, name):
        for ns in self.stack:
            if name in ns:
                return True
        return False


class CLVisitor(NodeVisitor):
    def __init__(self):
        super(CLVisitor, self).__init__()
        self._indent = 0
        self.s = StringIO()
        self.space_needed = False
        self.nss = NamespaceStack()
        self.in_classdef = False

    def code(self):
        return self.s.getvalue()

    def indent(self, ns=False):
        self._indent += 2
        if ns:
            self.nss.push_new_namespace()

    def dedent(self, ns=False):
        self._indent -= 2
        if ns:
            namespace = self.nss.pop_namespace()
            for _ in range(namespace.let_count):
                self.end_paren()
                self.dedent()

    def start_paren(self):
        if self.s.tell() > 0:
            self.s.seek(self.s.tell() - 1)
            if self.s.read(1) == '\n':
                print(' ' * self._indent, end='', file=self.s)
        print('(', end='', file=self.s)

    def end_paren(self):
        self.s.seek(self.s.tell() - 1)
        if self.s.read(1) in ' \n':
            self.s.seek(self.s.tell() - 1)
        print(')', end='\n', file=self.s)

    def p(self, *args):
        self.s.seek(self.s.tell() - 1)
        if self.s.read(1) == '\n':
            print(' ' * self._indent, end='', file=self.s)
        print(*args, end=' ', file=self.s)

    def nl(self):
        print(file=self.s)

    def d(self, node):
        print(' ' * self._indent,
              type(node).__name__,
              vars(node),
              file=self.s)

    def generic_visit(self, node):
        self.d(node)
        NodeVisitor.generic_visit(self, node)

    def visit_children(self, node):
        for child in iter_child_nodes(node):
            self.visit(child)

    def visit_Name(self, node):
        if node.id == 'True':
            self.p('t')
        elif node.id == 'False':
            self.p('nil')
        else:
            self.p(node.id)

    def visit_Load(self, node):
        self.d(node)

    def visit_Expr(self, node):
        self.visit_children(node)

    def visit_Assign(self, node):
        name = node.targets[0].id
        seen = name in self.nss
        if not seen:
            self.nss.add(node.targets[0].id)
            self.start_paren()
            self.p('let')
            self.indent()
            self.start_paren()
            self.start_paren()
            self.visit(node.targets[0])
            self.visit(node.value)
            self.end_paren()
            self.end_paren()
            self.nss.cns.let_count += 1
        elif seen:
            self.start_paren()
            self.p('setf')
            self.visit(node.targets[0])
            self.visit(node.value)
            self.end_paren()

    def visit_Call(self, node):
        self.start_paren()
        # range_builtin
        if node.func.id == 'range':
            self.builtin_range(node)
        else:
            self.visit_children(node)
        self.end_paren()

    def visit_FunctionDef(self, node):
        self.start_paren()
        if self.in_classdef:
            self.p('defmethod', node.name)
        else:
            self.p('defun', node.name)
        self.indent(ns=True)
        self.visit_children(node)
        self.dedent(ns=True)
        self.end_paren()

    def visit_ClassDef(self, node):
        self.start_paren()
        self.p('defclass')
        self.p(node.name)
        self.in_classdef = node.name
        self.indent()
        self.start_paren()
        for base in node.bases:
            self.visit(base)
        self.end_paren()
        self.end_paren()
        self.dedent()
        self.visit_children(node)
        #self.d(node)

    def visit_Pass(self, node):
        pass

    def visit_arguments(self, node):
        self.indent()
        self.start_paren()
        for arg in node.args:
            if self.in_classdef and arg.arg == 'self':
                self.p('self')
                self.p(self.in_classdef)
            else:
                self.p(arg.arg)
        self.end_paren()
        self.dedent()

    def visit_Return(self, node):
        self.start_paren()
        self.p('return')
        self.visit_children(node)
        self.end_paren()

    def visit_Str(self, node):
        self.p('"' + node.s + '"')

    def visit_Num(self, node):
        self.p(node.n)

    def visit_BinOp(self, node):
        self.start_paren()
        self.visit(node.op)
        self.visit(node.left)
        self.visit(node.right)
        self.end_paren()

    def visit_Add(self, node):
        self.p('+')

    def visit_Mult(self, node):
        self.p('*')

    def visit_Sub(self, node):
        self.p('-')

    def visit_Div(self, node):
        self.p('/')

    def visit_Module(self, node):
        self.nss.push_new_namespace()
        self.visit_children(node)

    def visit_Dict(self, node):
        self.start_paren()
        self.p('let')
        self.indent()
        self.start_paren()
        self.start_paren()
        self.p('_h')
        self.start_paren()
        self.p("make-hash-table :test 'equal")
        self.end_paren()
        self.end_paren()
        self.end_paren()
        self.start_paren()
        self.p('setf')
        self.indent()
        for key, value in zip(node.keys, node.values):
            self.start_paren()
            self.p('gethash')
            self.visit(key)
            self.p('_h')
            self.end_paren()
            self.visit(value)
        self.end_paren()
        self.dedent()
        self.p('_h')
        self.end_paren()
        self.dedent()

    def visit_List(self, node):
        self.start_paren()
        for elt in node.elts:
            self.visit(elt)
        self.end_paren()

    def visit_For(self, node):
        '''
        Simplest CL case:
        (loop for i in '(1 2 3) do (print i))

        Range type loops:
        (loop for x in (loop for i from 1 to 3 collect i)
            do (print x)))
        '''
        self.start_paren()
        self.p('loop for')
        self.p(node.target.id)
        self.p("in")
        if node.iter.__class__.__name__ == 'List':
            self.p("'")
        self.visit(node.iter)
        self.p('do')
        self.indent()
        for elt in node.body:
            self.visit(elt)
        self.dedent()
        self.end_paren()

    def builtin_range(self, node):
        self.p('loop for i from')
        args = node.args
        if len(args) == 1:
            self.p('0')
            self.p('below')
            self.visit(args[0])
        else:
            self.visit(args[0])
            self.p('below')
            self.visit(args[1])
        self.p('collect i')


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Python to CL compiler')
    parser.add_argument('fn', help='Filename')

    args = parser.parse_args()

    with open(args.fn) as f:
        code = f.read()

    syntax_tree = parse(code)

    parser = CLVisitor()
    parser.visit(syntax_tree)

    print(parser.code())
