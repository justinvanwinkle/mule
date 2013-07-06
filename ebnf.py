# This module tries to implement ISO 14977 standard with pyparsing.
# pyparsing version 1.1 or greater is required.

# ISO 14977 standardize The Extended Backus-Naur Form(EBNF) syntax.
# You can read a final draft version here:
# http://www.cl.cam.ac.uk/~mgk25/iso-ebnf.html

from pyparsing import And
from pyparsing import CharsNotIn
from pyparsing import Forward
from pyparsing import Group
from pyparsing import Literal
from pyparsing import NotAny
from pyparsing import OneOrMore
from pyparsing import Optional
from pyparsing import Or
from pyparsing import Suppress
from pyparsing import Word
from pyparsing import ZeroOrMore
from pyparsing import alphanums
from pyparsing import alphas
from pyparsing import delimitedList
from pyparsing import nums
from pyparsing import White


all_names = '''
integer
meta_identifier
terminal_string
optional_sequence
repeated_sequence
grouped_sequence
syntactic_primary
syntactic_factor
syntactic_term
single_definition
definitions_list
syntax_rule
syntax
'''.split()

integer = Word(nums)
newline = Suppress('NEWLINE')
zero_or_more = Optional('*')
one_or_more = Optional('+')
repeat = zero_or_more | one_or_more
meta_identifier = Word(alphas, alphanums + '_')
terminal_string = (Suppress("'") + CharsNotIn("'") + Suppress("'") ^
                   Suppress('"') + CharsNotIn('"') + Suppress('"'))
definitions_list = Forward()
optional_sequence = Suppress('[') + definitions_list + Suppress(']')
repeated_sequence = Suppress('{') + definitions_list + Suppress('}')
grouped_sequence = (Suppress('(') +
                    definitions_list +
                    Suppress(')') +
                    repeat)
syntactic_primary = Or([optional_sequence,
                        repeated_sequence,
                        grouped_sequence,
                        meta_identifier,
                        terminal_string,
                        newline])
syntactic_factor = Optional(integer + Suppress('*')) + syntactic_primary
syntactic_term = syntactic_factor + Optional(Suppress('-') + syntactic_factor)
single_definition = delimitedList(syntactic_term, ',')
definitions_list << delimitedList(single_definition, '|')
syntax_rule = (meta_identifier +
               Suppress('=') +
               definitions_list +
               Suppress(';'))

ebnfComment = ("(*" +
               ZeroOrMore(CharsNotIn("*") | ("*" + ~Literal(")"))) +
               "*)").streamline().setName("ebnfComment")

syntax = OneOrMore(syntax_rule)
syntax.ignore(ebnfComment)


def do_integer(str, loc, toks):
    return int(toks[0])


def do_meta_identifier(str, loc, toks):
    if toks[0] in symbol_table:
        return symbol_table[toks[0]]
    else:
        undefined.add(toks[0])
        symbol_table[toks[0]] = Forward()
        return symbol_table[toks[0]]


def do_terminal_string(str, loc, toks):
    return Literal(toks[0])


def do_optional_sequence(str, loc, toks):
    return Optional(toks[0])


def do_newline(str, loc, toks):
    return toks[0]


def do_repeated_sequence(str, loc, toks):
    return ZeroOrMore(toks[0])


def do_grouped_sequence(str, loc, toks):
    return Group(toks[0])


def do_syntactic_primary(str, loc, toks):
    return toks[0]


def do_syntactic_factor(str, loc, toks):
    if len(toks) == 2:
        # integer * syntactic_primary
        return And([toks[1]] * toks[0])
    else:
        # syntactic_primary
        return [toks[0]]


def do_syntactic_term(str, loc, toks):
    if len(toks) == 2:
        # syntactic_factor - syntactic_factor
        return NotAny(toks[1]) + toks[0]
    else:
        # syntactic_factor
        return [toks[0]]


def do_single_definition(str, loc, toks):
    toks = toks.asList()
    if len(toks) > 1:
        # syntactic_term , syntactic_term , ...
        return And(toks)
    else:
        # syntactic_term
        return [toks[0]]


def do_definitions_list(str, loc, toks):
    toks = toks.asList()
    if len(toks) > 1:
        # single_definition | single_definition | ...
        return Or(toks)
    else:
        # single_definition
        return [toks[0]]


def do_syntax_rule(str, loc, toks):
    # meta_identifier = definitions_list ;
    print('****', str, '****')
    assert toks[0].expr is None, "Duplicate definition"
    toks[0] << toks[1]
    return [toks[0]]


def do_syntax(str, loc, toks):
    # syntax_rule syntax_rule ...
    return symbol_table

symbol_table = {}


undefined = set()

for name in all_names:
    expr = vars()[name]
    action = vars()['do_' + name]
    expr.setName(name)
    expr.setParseAction(action)
    #~ expr.setDebug()


def parse(ebnf):
    symbol_table.clear()
    parsed = syntax.parseString(ebnf)
    table = parsed[0]
    for name in sorted(table):
        print(name, table[name])
    print(sorted(undefined))
    # assert len(undefined) == 0, "Missing definition"
    for name in table:
        expr = table[name]
        expr.setName(name)
        #~ expr.setDebug()
    return table

parse(open('grammar.ebnf').read())
