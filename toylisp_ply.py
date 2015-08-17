import ply.lex as lex
import ply.yacc as yacc
from interpreter import atom, eval, lispify

# Lexing

tokens = ('ATOM', 'QUOTE', 'LPAREN', 'RPAREN')


def t_ATOM(t):
    r'[^\s\)\(\']+'
    t.value = atom(t.value)
    return t

t_QUOTE = r'\''
t_LPAREN = r'\('
t_RPAREN = r'\)'

t_ignore = ' \t\n'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lex.lex()

# Parsing


def p_expr(p):
    '''expr : ATOM
            | list
            | QUOTE expr'''
    p[0] = ['quote', p[2]] if (len(p) == 3) else p[1]


def p_list(p):
    '''list : LPAREN seq RPAREN'''
    p[0] = p[2]


def p_seq(p):
    ''' seq :
            | expr seq'''
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = [p[1]] + p[2]


def p_error(p):
    print("Syntax error in input!")

yacc.yacc()

rep = lambda s: lispify(eval(yacc.parse(s)))

if __name__ == "__main__":
    rep('''
    (define fact
        (lambda (n)
            (cond ((eq? n 1) 1)
                  (else (* n (fact (- n 1)))))))''')
    print(rep("(fact 5)"))
