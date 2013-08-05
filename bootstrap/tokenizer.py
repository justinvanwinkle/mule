#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function

from pratt import PrattParser


class MuleTokenizer(PrattParser):
    def __init__(self, code):
        super(MuleTokenizer, self).__init__(self, code)

    @property
    def tokens(self):
        return self.code
