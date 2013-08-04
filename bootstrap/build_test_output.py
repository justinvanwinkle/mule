#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from glob import glob
from os import mkdir
from os.path import split as splitpath
from os.path import splitext

import pratt

if __name__ == '__main__':
    try:
        mkdir('OUTPUT')
    except OSError:
        pass

    pratt.debug = False
    in_fns = glob('test_cases/*.py')
    for in_fn in in_fns:
        in_fn_name = splitext(splitpath(in_fn)[1])[0]
        output_fn = 'OUTPUT/' + in_fn_name + '.lisp'
        with open(output_fn, 'w') as f:
            try:
                output = pratt.parse_file(in_fn).cl()
                f.write(output)
            except Exception, e:
                print in_fn, e
