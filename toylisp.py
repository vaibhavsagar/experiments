'''
Help and guidance from:
1. http://norvig.com/lispy.html
2. http://theory.stanford.edu/~amitp/yapps/yapps-doc/node2.html for grammar
3. http://www.dabeaz.com/ply/ply.html

'''

import ply.lex  as lex
import ply.yacc as yacc

tokens = (
    'ATOM',
    'QUOTE',
    'LPAREN',
    'RPAREN'
)

t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_QUOTE   = r'\''
def t_ATOM(t):
    r'[^\s\)\(\']+'
    t.value=atom(t.value)
    return t

t_ignore  = ' \t\n'
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lex.lex()


def p_expr(p):
    '''expr : ATOM
            | list
            | QUOTE expr'''
    p[0] = ['quote', p[2]] if (len(p) == 3) else p[1]

def p_list(p):
    '''list : LPAREN seq RPAREN'''
    p[0]=p[2]

def p_seq(p):
    ''' seq :
            | expr seq'''
    if len(p)==1: p[0]=[]
    else: p[0]=[p[1]]+p[2]
    
def p_error(p):
    print("Syntax error in input!")

def atom(token):
    "Numbers become numbers; every other token is a symbol."
    try: return int(token)
    except ValueError: return str(token)

class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms,args))
        self.outer = outer
    def find(self, var):
        "Find the innermost Env where var appears."
        return self if var in self else self.outer.find(var)

def add_globals(env):
    "Add some Scheme standard procedures to an environment."
    import math, operator as op
    env.update(vars(math)) # sin, sqrt, ...
    env.update(
     {'+':op.add, '-':op.sub, '*':op.mul, '/':op.truediv, 'not':op.not_,
      '>':op.gt, '<':op.lt, '>=':op.ge, '<=':op.le, '=':op.eq, 
      'equal?':op.eq, 'eq?':op.is_, 'length':len, 'cons':lambda x,y:[x]+y,
      'car':lambda x:x[0],'cdr':lambda x:x[1:], 'append':op.add,  
      'list':lambda *x:list(x), 'list?': lambda x:isinstance(x,list), 
      'null?':lambda x:x==[], 'symbol?':lambda x: isinstance(x, str)})
    return env

global_env = add_globals(Env())

def eval(x, env=global_env):
    "Evaluate an expression in an environment."
    if isinstance(x, str):         # variable reference
        return env.find(x)[x]
    elif not isinstance(x, list):  # constant literal
        return x                
    elif x[0] == 'quote':          # (quote exp)
        (_, exp) = x
        return exp
    elif x[0] == 'if':             # (if test conseq alt)
        (_, test, conseq, alt) = x
        return eval((conseq if eval(test, env) else alt), env)
    elif x[0] == 'set!':           # (set! var exp)
        (_, var, exp) = x
        env.find(var)[var] = eval(exp, env)
    elif x[0] == 'define':         # (define var exp)
        (_, var, exp) = x
        env[var] = eval(exp, env)
    elif x[0] == 'lambda':         # (lambda (var*) exp)
        (_, vars, exp) = x
        return lambda *args: eval(exp, Env(vars, args, env))
    elif x[0] == 'begin':          # (begin exp*)
        for exp in x[1:]:
            val = eval(exp, env)
        return val
    else:                          # (proc exp*)
        exps = [eval(exp, env) for exp in x]
        proc = exps.pop(0)
        return proc(*exps)



yacc.yacc()
rep = lambda s: eval(yacc.parse(s))
if __name__=="__main__": print(eval(yacc.parse("(cons 3 '())")))
