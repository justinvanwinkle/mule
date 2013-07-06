from __future__ import print_function
from __future__ import unicode_literals

from ast import parse
from ast import iter_child_nodes
from ast import iter_fields
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


class Node(object):
    def __init__(self, node):
        self.node = node
        self.fields = dict(iter_fields(node))

    def map_field(self, field_name):
        value = self.fields[field_name]
        if isinstance(value, (str, int, float)):
            return value

        try:
            return map_nodes(value)
        except TypeError:
            return map_node(value)

    def render(self):
        return "Can't render %s ::: %s" % (self.node,
                                           list(iter_fields(self.node)))

    def children(self):
        return ()

    def __str__(self):
        return '\n'.join(self.render())


class Module(Node):
    def __init__(self, node):
        super(Module, self).__init__(node)
        self.body = self.map_field('body')

    def render(self):
        return [x.render() for x in self.body]


class Assign(Node):
    def __init__(self, node):
        super(Assign, self).__init__(node)
        self.targets = self.map_field('targets')
        self.value = self.map_field('value')

    def render(self):
        value_body = self.value.render()

        if isinstance(self.targets, tuple):
            var_list = ' '.join(t.render() for t in self.targets)
            s = '(multiple-value-bind (' + var_list + ') ' + value_body + ')'
            s = s % self.targets

        template = '(setf %s %s)'
        rendered_targets = self.targets[0].render()
        return template % (rendered_targets, value_body)


class Tuple(Node):
    def __init__(self, node):
        super(Tuple, self).__init__(node)
        self.elts = self.map_field('elts')
        self.ctx = self.map_field('ctx')

    def render(self):
        return '(' + ' '.join(str(elt) for elt in self.elts) + ')'


class Name(Node):
    def __init__(self, node):
        super(Name, self).__init__(node)
        self.ctx = self.map_field('ctx')
        self.id = self.map_field('id')

    def render(self):
        return self.id


class Store(Node):
    pass


class Num(Node):
    def __init__(self, node):
        super(Num, self).__init__(node)
        self.n = self.map_field('n')

    def render(self):
        return self.n

node_mapping = dict((cls.__name__, cls) for cls in
                    (Module, Assign, Tuple, Name, Store, Num))


def map_node(node):
    return node_mapping.get(node.__class__.__name__, Node)(node)


def map_nodes(nodes):
    result = []
    for node in nodes:
        result.append(map_node(node))
    return result


class CLVisitor(NodeVisitor):
    def __init__(self):
        super(CLVisitor, self).__init__()

    def visit(self, node):
        cls = node_mapping.get(node.__class__.__name__, Node)
        self.visit_children(node)

        return cls(node)

    def generic_visit(self, node):
        NodeVisitor.generic_visit(self, node)

    def visit_children(self, node):
        for child in iter_child_nodes(node):
            self.visit(child)


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Python to CL compiler')
    parser.add_argument('fn', help='Filename')

    args = parser.parse_args()

    with open(args.fn) as f:
        code = f.read()

    syntax_tree = parse(code)

    parser = CLVisitor()
    module = parser.visit(syntax_tree)
    for el in module.render():
        print(el)
    #print(parser.code())
