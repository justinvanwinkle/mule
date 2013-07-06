# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from glob import glob
import re

import pytest

from pratt import scanner

test_files = [(x,) for x in glob('test_cases/*.py')]


def get_name(filename):
    return re.compile(
        'test_cases/test_(\w+).py').match(filename).group(1)


tokens = {
    'add_names': [('NAME', 'def'),
                  ('NAME', 'test'),
                  ('NAME', 'x'),
                  ('INTEGER', '2'),
                  ('NAME', 'y'),
                  ('INTEGER', '2'),
                  ('NAME', 'x'),
                  ('OP', '+'),
                  ('NAME', 'y')],
    'arithmetic_operators': None,
    'assign': None,
    'class': None,
    'defun': None,
    'fields': None,
    'global_scope': None,
    'loop': None}


@pytest.mark.parametrize(("fn"), test_files)
def test_tokenizer(fn):
    with open(fn) as f:
        assert tokens[get_name(fn)] == scanner.scan(f.read())
