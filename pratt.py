# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import re


def tok(token_type):
    def _(scanner, token):
        return token_type, token
    return _


scanner = re.Scanner([
    (r"(    )+", tok('INDENT')),
    ("class", tok('CLASS')),
    ("def", tok('DEF')),
    ("for", tok('FOR')),
    ("while", tok('WHILE')),
    (r"[^\W\d]\w*", tok('NAME')),
    (r'\(', tok('LPAREN')),
    (r'\)', tok('RPAREN')),
    (r'\[', tok('LBRACKET')),
    (r'\]', tok('RBRACKET')),
    (r'\{', tok('LBRACE')),
    (r'\}', tok('RBRACE')),
    (r',', tok('COMMA')),
    (r':', tok('COLON')),
    (r'=', tok('ASSIGN')),
    (r"-?[0-9]+\.[0-9]+([eE]-?[0-9]+)?", tok('FLOAT')),
    (r"-?[0-9]+", tok('INTEGER')),
    (r"\+", tok('PLUS')),
    (r"\-", tok('MINUS')),
    (r"\*", tok('MULTIPLY')),
    (r"/", tok('DIVIDE')),
    (r"'''(.*?)'''", tok('MULTI-LINE-STRING')),
    (r'"""(.*?)"""', tok('MULTI-LINE-STRING')),
    (r"'(.*?)'", tok('STRING')),
    (r'"(.*?)"', tok('STRING')),
    (r'\n', tok('NEWLINE')),
    (r"\s+", None),
])


def tokenize(code):
    return scanner.scan(code)


def tokenize_file(fn):
    with open(fn) as f:
        return tokenize(f.read())
