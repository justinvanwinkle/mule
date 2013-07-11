#from ast import parse
from glob import glob

#from compile import CLVisitor
from pratt import tokenize_file
from pprint import pprint


def _get_test_params():
    params = []
    for test_fn in sorted(glob('test_cases/*.py')):
        params.append((test_fn,))
    return params


def write_tokens(test_fn):
    if test_fn.startswith('test_cases/_'):
        return

    token_fn = test_fn[:-3] + '.token'

    tokens = tokenize_file(test_fn)
    with open(token_fn, 'w') as f:
        pprint(tokens, f)


if __name__ == '__main__':
    for fn in _get_test_params():
        write_tokens(fn)
