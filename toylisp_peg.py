''' Some code from https://github.com/keleshev/mini/blob/master/mini.py'''

from parsimonious.grammar import Grammar
from interpreter import Env, add_globals
from interpreter import eval as interpreter_eval


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
        return interpreter_eval(e, env)

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

if __name__ == "__main__":
    L = Lisp()
    L.eval(L.ast('''
    (define fact
        (lambda (n)
            (cond ((eq? n 1) 1)
                  (else (* n (fact (- n 1)))))))'''))
    print(L.eval(L.ast("(fact 5)")))
