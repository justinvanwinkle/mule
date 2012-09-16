from ast import parse
from glob import glob

from compile import CLVisitor


def pytest_generate_tests(metafunc):
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
        metafunc.addcall(
            funcargs=dict(case_fn=test_fn,
                          gen_code=parser.code().strip(),
                          target_code=lisp_code.strip()))


def test_example_case(case_fn, gen_code, target_code):
    print(case_fn)
    if gen_code != target_code:
        print('***generated code***')
        print(gen_code)
        print('***target code***')
        print(target_code)
    assert gen_code == target_code
