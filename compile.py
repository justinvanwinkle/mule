import sys
import readline

import ply.yacc as yacc


precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
)


def flatten(x):
    result = []
    for el in x:
        if hasattr(el, "__iter__") and not isinstance(el, basestring):
            result.extend(flatten(el))
        else:
            result.append(el)
    return result


def p_error(p):
    print '***** Syntax Error in input! ******'
    

def p_10(p):
    '''statement : NEWLINE'''
    pass


def p_6(p):
    '''statement : assignment NEWLINE'''
    p[0] = p[1]
    pass


def p_7(p):
    '''statement : expression NEWLINE'''
    p[0] = p[1]


def p_8(p):
    '''statement : funcdef NEWLINE'''
    p[0] = p[1]


def p_11(p):
    '''statement : RETURN expression NEWLINE'''
    #p[0] = ('return', p[2])
    # Note: We need to figure out what to do with return
    p[0] = p[2]


def p_12(p):
    '''statement : END NEWLINE'''
    p[0] = 'END'


def p_17(p):
    '''statement : DOTIMES IDENT NUMBER COLON NEWLINE'''
    p[0] = ('dotimes', (p[2], p[3]), 'DROPNEXT')


def p_14(p):
    '''funcdef : DEF IDENT LPAREN params RPAREN COLON'''
    p[0] = ('defun', p[2], (p[4]), 'DROPNEXT')


def p_15(p):
    '''params : IDENT'''
    p[0] = (p[1],)


def p_16(p):
    '''params : params COMMA IDENT'''
    p[0] = p[1] + (p[3], )


def p_5(p):
    '''assignment : IDENT EQUALS expression'''
    p[0] = ('setq', p[1], p[3])


def p_1(p):
    '''expression : expression PLUS expression'''
    p[0] = ('+', p[1], p[3])


def p_2(p):
    '''expression : LPAREN expression RPAREN'''
    p[0] = p[2]


def p_3(p):
    '''expression : NUMBER'''
    p[0] = p[1]


def p_13(p):
    '''expression : IDENT'''
    p[0] = p[1]


def p_4(p):
    '''expression : expression TIMES expression'''
    p[0] = ('*', p[1], p[3])


def p_24(p):
    '''expression : expression DIVIDE expression'''
    p[0] = ('/', p[1], p[3])


def p_25(p):
    '''expression : expression MINUS expression'''
    p[0] = ('-', p[1], p[3])


def p_18(p):
    '''expression : IDENT LPAREN arglist RPAREN'''
    p[0] = (p[1],) + p[3]


def p_22(p):
    '''arglist : arg'''
    p[0] = (p[1], )


def p_19(p):
    '''arglist : arglist COMMA arg'''
    p[0] = p[1] + (p[3], )


def p_20(p):
    '''arg : NUMBER'''
    p[0] = p[1]


def p_21(p):
    '''arg : IDENT'''
    p[0] = p[1]


def p_23(p):
    '''arg : expression'''
    p[0] = p[1]


def lispify(node):
    if type(node) == type(1):
        return '{}'.format(node)
    if node == 'END':
        return ')'
    retval = '('
    leave_open = False
    for x in node:
        if type(x) == type((1,2,3)):
            retval += lispify(x)
        elif x == 'DROPNEXT': # TODO: remove newline mode
            leave_open = True;
        else:
            retval += ' {} '.format(x)
    if not leave_open:
        # This is a very lame situation
        # arising out of using line by line 
        # mode. It will be removed in
        # next iteration
        retval += ')'
    return retval


if __name__ == '__main__':
    import argparse

    arg_parser = argparse.ArgumentParser(
        description='Experimental Language to CL compiler')
    arg_parser.add_argument('fn', help='Filename')

    args = arg_parser.parse_args()
    from xlex import lexer, tokens

    parser = yacc.yacc()

    with open(args.fn) as f:
        for i, line in enumerate(f.readlines()):
            
            result = parser.parse(line)
            if not result:
                continue
            print lispify(result)

