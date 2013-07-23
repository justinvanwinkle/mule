from __future__ import unicode_literals
from pratt import parse_file

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Python to CL compiler')
    parser.add_argument('fn', help='Filename')
    parser.add_argument('--ast', action='store_true', help='Dump ast')

    args = parser.parse_args()
    result = parse_file(args.fn)
    if args.ast:
        print result
    else:
        print result.cl()
