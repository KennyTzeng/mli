# mli
A mini-Lisp interpreter implemented by PLY(Python Lex Yacc) module.
Developed in Python 3.6.1

# Usage
py mli.py

# Example
>>> (print-num (+ 1 2 3 4 5) )
15
>>> (define x 6)
>>> (print-num (+ x (+ 1 2 3 4 5) ) )
21
