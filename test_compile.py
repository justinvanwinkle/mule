#from ast import parse
from glob import glob
import pytest

#from compile import CLVisitor
from pratt import tokenize_file
from pprint import pprint


def _get_test_params():
    params = []
    for test_fn in sorted(glob('test_cases/*.py')):
        params.append((test_fn,))
    return params


@pytest.mark.parametrize(("test_fn",), _get_test_params())
def test_tokenizer(test_fn):
    if test_fn.startswith('test_cases/_'):
        return

    token_fn = test_fn[:-3] + '.token'
    good_tokens = eval(open(token_fn).read())

    tokens = tokenize_file(test_fn)
    pprint(tokens)
    assert tokens == good_tokens


# @pytest.mark.parametrize(("test_fn",), _get_test_params())
# def test_code_generation(test_fn):
#         if test_fn.startswith('test_cases/_'):
#             return
#         lisp_fn = test_fn[:-3] + '.lisp'

#         f = open(test_fn)
#         lf = open(lisp_fn)
#         code = f.read()
#         lisp_code = lf.read()
#         f.close()
#         lf.close()

#         parser = CLVisitor()
#         tree = parse(code)
#         parser.visit(tree)
#         test_example_case(test_fn, parser.code().strip(), lisp_code.strip())


# def test_example_case(case_fn, gen_code, target_code):
#     print(case_fn)
#     if gen_code != target_code:
#         print('***generated code***')
#         print(gen_code)
#         print('***target code***')
#         print(target_code)
#     assert target_code == gen_code
