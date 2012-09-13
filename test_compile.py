from ast import parse

from compile import CLVisitor


def test_add():
    tree = parse('1 + 1')
    parser = CLVisitor()
    parser.visit(tree)
    assert parser.code() == '(+ 1 1)\n'


def test_add_names():
    tree = parse('x + y')
    parser = CLVisitor()
    parser.visit(tree)
    assert parser.code() == '(+ x y)\n'


def test_defun():
    pycode = '''\
def test(x, y):
    return x + y
'''

    tree = parse(pycode)
    parser = CLVisitor()
    parser.visit(tree)
    assert parser.code() == '''\
(defun test (x y)
(return (+ x y)))
'''
