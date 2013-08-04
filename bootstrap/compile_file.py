from __future__ import unicode_literals
from pratt import parse_file
from os.path import split
from os.path import splitext

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Python to CL compiler')
    parser.add_argument('fn', help='Filename')
    parser.add_argument('--ast', action='store_true', help='Dump ast')

    args = parser.parse_args()
    fn = splitext(split(args.fn)[1])[0]
    result = parse_file(args.fn, filename=fn)
    print '\n\n'
    if args.ast:
        print result
    else:
        print result.cl()
