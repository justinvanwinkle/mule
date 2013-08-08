#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals
from __future__ import print_function

from pratt import PrattParser

from sys import stdout


class Namespace(object):
    def __init__(self, return_name=None, class_top_level=False):
        self.s = set()
        self.let_count = 0
        self.return_name = return_name
        self.class_top_level = class_top_level

    def add(self, name):
        self.s.add(name)

    def __contains__(self, name):
        return name in self.s

    def __repr__(self):
        return '<NAMESPACE return_name=%s class_top_level=%s %s>' % (
            self.return_name,
            self.class_top_level,
            ', '.join(self.s))


class NamespaceStack(object):
    def __init__(self):
        self.stack = []

    @property
    def cns(self):
        if self.stack:
            return self.stack[-1]
        else:
            return None

    @property
    def top_level(self):
        return self.depth == 0

    @property
    def class_top_level(self):
        if self.cns:
            return self.cns.class_top_level

    @property
    def return_name(self):
        for ns in reversed(self.stack):
            if ns.return_name is not None:
                return ns.return_name

    @property
    def depth(self):
        return len(self.stack) - 1

    def push_new(self, return_name=None, class_top_level=False):
        ns = Namespace(return_name=return_name,
                       class_top_level=class_top_level)
        self.stack.append(ns)

    def pop(self):
        lost_namespace = self.stack.pop()
        return lost_namespace

    def add(self, name):
        self.cns.add(name)

    def __contains__(self, name):
        for ns in self.stack:
            if name in ns:
                return True
        return False

    def __repr__(self):
        return '<NamespaceStack depth=%s cns=%s stack=%s>' % (
            self.depth,
            self.cns,
            self.stack)


class MuleParser(PrattParser):
    def __init__(self, code, token_defs, filename=None):
        PrattParser.__init__(self, code, token_defs, filename=filename)
        self.ns = NamespaceStack()

    def _munge_tokens(self, tokens):
        from token_defs import Block
        from token_defs import Endblock
        from token_defs import Module

        class Fake:
            name = None

        def next_tok(pos):
            if pos + 1 >= len(tokens):
                return Fake()
            return tokens[pos + 1]

        def prev_tok(pos):
            if pos < 1:
                return Fake()
            return tokens[pos - 1]

        def is_significant(pos):
            if pos > 1:
                if prev_tok(pos).name != 'NEWLINE':
                    return False
            else:
                return False

            if next_tok(pos).name != 'NEWLINE':
                return True
            return False

        def change_indent(new_indent):
            if new_indent > current_indent + 1:
                raise Exception('Indented too much %s' % index)
            elif new_indent == current_indent + 1:
                new_tokens.append(Block())
            elif new_indent < current_indent:
                for _ in range(current_indent - new_indent):
                    new_tokens.append(Endblock())

        new_tokens = [Module(), Block()]
        current_indent = 0

        for index, token in enumerate(tokens):
            new_indent = current_indent
            if token.name == 'WHITESPACE':
                if not is_significant(index):
                    continue
                new_indent = len(token.value) / 4
            elif token.name == 'NEWLINE':
                if prev_tok(index).name != 'NEWLINE':
                    new_tokens.append(token)
                if next_tok(index).name not in ('NEWLINE', 'WHITESPACE'):
                    new_indent = 0
            else:
                new_tokens.append(token)
            change_indent(new_indent)
            current_indent = new_indent

        new_tokens.append(Endblock())
        return new_tokens

    def parse_rest_of_body(self):
        forms = []
        while self.watch('ENDBLOCK', consume=False):
            while self.maybe_match('NEWLINE'):
                pass
            form = self.expression()
            forms.append(form)
            while self.maybe_match('NEWLINE'):
                pass
        return forms


if __name__ == '__main__':
    import argparse
    from os.path import splitext
    from os.path import split

    from token_defs import all_ops

    argparser = argparse.ArgumentParser(description='Python to CL compiler')
    argparser.add_argument('mule_fn', help='input file')
    argparser.add_argument('lisp_fn', nargs='?', help='output file')
    argparser.add_argument('-d', '--debug',
                           action='store_true',
                           help='debug output')
    args = argparser.parse_args()

    fn = splitext(split(args.mule_fn)[1])[0]
    with open(args.mule_fn) as f:
        code = f.read()

    try:
        mule_parser = MuleParser(code, all_ops, filename=fn)
        if args.debug:
            mule_parser.debug = True
        result = mule_parser.parse()
    except Exception:
        import tracebackturbo
        import sys
        print(tracebackturbo.format_exc(with_vars=True))
        sys.exit(1)
    f = stdout
    if args.lisp_fn:
        f = open(args.lisp_fn, 'w')
    else:
        print('\n\n')
    print(result.cl(), file=f)
    if args.lisp_fn:
        f.close()
