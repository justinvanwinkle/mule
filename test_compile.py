#from ast import parse
from glob import glob
import pytest

from pratt import parse_file


def _get_test_params():
    params = []
    for test_fn in sorted(glob('test_cases/*.py')):
        params.append((test_fn,))
    return params


@pytest.mark.parametrize(("test_fn",), _get_test_params())
def test_code_generation(test_fn):
        if test_fn.startswith('test_cases/_'):
            return
        lisp_fn = test_fn[:-3] + '.lisp'

        output = parse_file(test_fn)
        with open(lisp_fn) as f:
            lisp_code = f.read()

        test_example_case(test_fn, output, lisp_code.strip())


def test_example_case(case_fn, gen_code, target_code):
    print(case_fn)
    if gen_code != target_code:
        print('***generated code***')
        print(gen_code)
        print('***target code***')
        print(target_code)
    assert target_code == gen_code
