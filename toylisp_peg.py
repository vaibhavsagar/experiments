''' Some code from https://github.com/halst/mini/blob/master/mini.py'''

from parsimonious.grammar import Grammar


class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, params=(), args=(), outer=None):
        self.update(zip(params, args))
        self.outer = outer

    def find(self, var):
        "Find the innermost Env where var appears."
        return self.get(var, self.outer.find(var) if self.outer else None)


def add_globals(env):
    "Add some Lisp standard procedures to an environment."
    import operator as op
    from functools import reduce
    reducer = lambda o: lambda *args: reduce(o, args)
    env.update({
        '+': reducer(op.add),
        '-': reducer(op.sub),
        '*': reducer(op.mul),
        '/': reducer(op.truediv),
        'not':  op.not_,
        '>': op.gt,
        '<': op.lt,
        '>=': op.ge,
        '<=': op.le,
        '=': op.eq,
        'eq?': op.eq,
        'cons': lambda x, y: [x]+y,
        'car': lambda x: x[0],
        'cdr': lambda x: x[1:],
        'atom?': is_atom,
        'else': True
    })
    return env

is_atom = lambda v: isinstance(v, str)
is_literal = lambda v: not isinstance(v, list)


def notbound(var):
    raise NameError("symbol '%s' is not bound to a value" % var)


class Lisp:

    def __init__(self, env=add_globals(Env())):
        self.env = env
        self.grammar = Grammar('\n'.join(
            v.__doc__ for k, v in vars(self.__class__).items()
            if '__' not in k and hasattr(v, '__doc__') and v.__doc__))

    def parse(self, source):
        return self.grammar['expr'].parse(source)

    def ast(self, source):
        node = self.parse(source) if isinstance(source, str) else source
        method = getattr(self, node.expr_name, lambda node, children: children)
        return method(node, [self.ast(n) for n in node])

    def eval(self, e, env=None):
        # Evaluate an expression in an environment
        if env is None:
            env = self.env
        if is_atom(e):          # variable reference
            value = env.find(e)
            return value if value is not None else notbound(e)
        elif is_literal(e):     # constant literal
            return e
        elif e[0] == 'quote':   # (quote exp)
            (_, exp) = e
            return exp
        elif e[0] == 'cond':    # (if test conseq alt)
            (_, *forms) = e
            for test, result in forms:
                if self.eval(test, env):
                    return self.eval(result, env)
        elif e[0] == 'define':  # (define var exp)
            (_, var, exp) = e
            env[var] = self.eval(exp, env)
        elif e[0] == 'lambda':  # (lambda (var*) exp)
            (_, vars, exp) = e
            return lambda *args: self.eval(exp, Env(vars, args, env))
        else:                   # (proc exp*)
            exps = [self.eval(exp, env) for exp in e]
            proc = exps.pop(0)
            return proc(*exps)

    def expr(self, node, children):
        '''expr = _ (list / quoted / atom) _'''
        return children[1][0]

    def quoted(self, node, children):
        '''quoted = "'" expr'''
        return ['quote', children[1]]

    def list(self, node, children):
        '''list = "(" expr* ")"'''
        return children[1]

    def atom(self, node, children):
        '''atom = ~r"[^\s\)\(\']+"'''
        try:
            result = int(node.text)
        except:
            result = node.text
        return result

    def _(self, node, children):
        '''_ = ~"\s*"'''


def lispify(value):
    return str(value) if is_literal(value) else "("+(" ".join(
        lispify(v) for v in value))+")"


if __name__ == "__main__":
    L = Lisp()
    L.eval(L.ast('''
    (define fact
        (lambda (n)
            (cond ((eq? n 1) 1)
                  (else (* n (fact (- n 1)))))))'''))
    print(L.eval(L.ast("(fact 5)")))
