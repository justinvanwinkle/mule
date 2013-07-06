# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import re


def tok(token_type):
    def _(scanner, token):
        return token_type, token
    return _


scanner = re.Scanner([
    (r"[^\W\d]\w*", tok('NAME')),
    (r"-?[0-9]+\.[0-9]+([eE]-?[0-9]+)?", tok('FLOAT')),
    (r"-?[0-9]+", tok('INTEGER')),
    (r"\+", tok('ADD')),
    (r"\s+", None),
])

print scanner.scan("name5 10 + 5 + 5.33")
