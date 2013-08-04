#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function

from os import walk
from os.path import exists
from os import mkdir
from os.path import join
from os.path import realpath
from shutil import rmtree
import sys
from subprocess import Popen
from subprocess import PIPE
import logging

import pratt

pretty_printer = realpath(join(__file__, '../', 'pretty-print'))


def build_tree(in_path, out_path, pretty=False):
    print('removing directory %s' % out_path)
    rmtree(out_path)
    for root, dirs, files in walk(in_path):
        full_out_path = join(out_path, root.partition('/')[2])
        if not exists(full_out_path):
            mkdir(full_out_path)
        for fn in files:
            in_fn = join(root, fn)
            if in_fn.endswith('.mule'):
                out_fn = join(full_out_path, fn[:-5] + '.lisp')
                with open(out_fn, 'w') as f:
                    try:
                        lisp_code = pratt.parse_file(in_fn).cl()
                        if pretty:
                            p = Popen(['sbcl', '--script', pretty_printer],
                                      stdin=PIPE, stdout=PIPE)
                            lisp_code, err = p.communicate(lisp_code)
                        f.write(lisp_code)
                    except Exception:
                        logging.exception('Fail')

                print(in_fn, out_fn, sep=' -> ', file=sys.stderr)


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("in_path", help="path to find .mule files")
    parser.add_argument("out_path", help="where to put .lisp files")
    parser.add_argument("--debug",
                        action='store_true',
                        help="verbose debug logging")
    parser.add_argument('--pretty',
                        action='store_true',
                        help="pretty print lisp forms")
    args = parser.parse_args()

    pratt.debug = args.debug
    build_tree(args.in_path, args.out_path, args.pretty)
    return 0


if __name__ == '__main__':
    sys.exit(main() != 0)
