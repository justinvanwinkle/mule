# pythonGrammarParser.py
#
# Copyright, 2006, by Paul McGuire
#

from pyparsing import *

grammar = open('grammar.ebnf').read()


class SemanticGroup(object):
    def __init__(self, contents):
        self.contents = contents
        while self.contents[-1].__class__ == self.__class__:
            self.contents = self.contents[:-1] + self.contents[-1].contents

    def __str__(self):
        return "%s(%s)" % (self.label,
                           " ".join([isinstance(c, str) and c or str(c)
                                     for c in self.contents]))


class OrList(SemanticGroup):
    label = "OR"
    pass


class AndList(SemanticGroup):
    label = "AND"
    pass


class OptionalGroup(SemanticGroup):
    label = "OPT"
    pass


class Atom(SemanticGroup):
    def __init__(self, contents):
        if len(contents) > 1:
            self.rep = contents[1]
        else:
            self.rep = ""
        if isinstance(contents, str):
            self.contents = contents
        else:
            self.contents = contents[0]

    def __str__(self):
        return "%s%s" % (self.rep, self.contents)


def makeGroupObject(cls):
    def groupAction(s, l, t):
        try:
            return cls(t[0].asList())
        except:
            return cls(t)
    return groupAction


symbol_table = {}

# bnf punctuation
LPAREN = Suppress("(")
RPAREN = Suppress(")")
LBRACK = Suppress("[")
RBRACK = Suppress("]")
ASSIGN = Suppress("=")
ALT_OP = Suppress("|")
SEMICOLON = Suppress(";")

# bnf grammar
ident = Word(alphanums + "_")
bnfToken = Word(alphanums + "_") + ~FollowedBy(":")
repSymbol = oneOf("* +")
bnfExpr = Forward()
optionalTerm = (Group(LBRACK + bnfExpr + RBRACK)
                .setParseAction(makeGroupObject(OptionalGroup)))
bnfTerm = (((bnfToken | quotedString | optionalTerm |
             (LPAREN + bnfExpr + RPAREN)) + Optional(repSymbol))
           .setParseAction(makeGroupObject(Atom)))
andList = (Group(bnfTerm + OneOrMore(bnfTerm))
           .setParseAction(makeGroupObject(AndList)))
bnfFactor = andList | bnfTerm
orList = (Group(bnfFactor + OneOrMore(ALT_OP + bnfFactor))
          .setParseAction(makeGroupObject(OrList)))
bnfExpr << (orList | bnfFactor)
bnfLine = ident + ASSIGN + bnfExpr + SEMICOLON

bnfComment = (("#" + restOfLine) |
              ('(*' + ZeroOrMore(CharsNotIn("*") |
              ("*" + ~Literal(")"))) + "*)"))

# build return tokens as a dictionary
bnf = Dict(OneOrMore(Group(bnfLine)))
bnf.ignore(bnfComment)

# bnf is defined, parse the grammar text
bnfDefs = bnf.parseString(grammar)

# correct answer is 78
# expected = 78
# assert len(bnfDefs) == expected, \
#     "Error, found %d BNF defns, expected %d" % (len(bnfDefs), expected)

# list out defns in order they were parsed (to verify accuracy of parsing)
# for k, v in bnfDefs:
#     print(k, "=", v)
# print()

# # list out parsed grammar defns
# # (demonstrates dictionary access to parsed tokens)
# for k in bnfDefs.keys():
#     print(k, "=", bnfDefs[k])

code = """\
for x in [1, 2, 3]:
    print(x)
"""
