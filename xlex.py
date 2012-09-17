# ------------------------------------------------------------
# xl-lex.py
#
# tokenizer for a simple experimental language that aims to
# compile to Common Lisp
# ------------------------------------------------------------
import ply.lex as lex

# List of token names.   This is always required
tokens = (
   'NUMBER',
   'PLUS',
   'MINUS',
   'TIMES',
   'DIVIDE',
   'LPAREN',
   'RPAREN',
   'IDENT',
   'COMMA',
   'COLON',
   'EQUALS',
   'NEWLINE'
)

# Reserved words
reserved = {
   'dotimes': 'DOTIMES',
   'if': 'IF',
   'else': 'ELSE',
   'while': 'WHILE',
   'elif': 'ELIF',
   'def': 'DEF',
   'end': 'END',
   'return': 'RETURN',
   'None': 'NONE',
   'for': 'FOR',
   'in': 'IN',
   'inline': 'INLINE'
}

tokens += tuple(reserved.values())

# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_DEF     = r'DEF'
t_END     = r'END'
t_COMMA   = r','
t_COLON   = r':'
t_EQUALS  = r'='


# A regular expression rule with some action code
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


# Rule for ident to change token type for reserved words
def t_IDENT(t):
    r'[a-zA-Z_][0-9a-zA-Z_]*'
    t.type = reserved.get(t.value, 'IDENT')
    return t


def t_NEWLINE(t):
    r'\n+'
    return t


# Ignore comments
def t_COMMENT(t):
    '\#.*'
    pass


# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'


# Error handling rule
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)


# Build the lexer
lexer = lex.lex(debug=0)
