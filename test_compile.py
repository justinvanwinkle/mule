from ast import parse
from glob import glob

from compile import CLVisitor


def test_run_test_cases():
    test_fns = glob('test_cases/*.py')
    for test_fn in test_fns:
        lisp_fn = test_fn[:-3] + '.lisp'

        f = open(test_fn)
        lf = open(lisp_fn)
        code = f.read()
        lisp_code = lf.read()
        f.close()
        lf.close()

        parser = CLVisitor()
        tree = parse(code)
        parser.visit(tree)
        if parser.code().strip() != lisp_code.strip():
            print(parser.code())
        print(test_fn, lisp_fn)
        assert parser.code().strip() == lisp_code.strip()
