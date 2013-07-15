from __future__ import unicode_literals
from pratt import parse_file

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Python to CL compiler')
    parser.add_argument('fn', help='Filename')

    args = parser.parse_args()

    print parse_file(args.fn)
