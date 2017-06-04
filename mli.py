# -----------------------------------------------------------------------------
# mli.py
#
# A miniLisp interpreter.
# -----------------------------------------------------------------------------\

import ply.lex as lex
import ply.yacc as yacc

tokens = (
			'NAME','NUMBER','PLUS','MINUS','TIMES','DIVIDE',
			'MOD','GREATER','SMALLER','EQUALS','LPAREN','RPAREN',
			'AND','OR','NOT',
		)

# Lex Tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_MOD	  = r'mod'
t_GREATER = r'>'
t_SMALLER = r'<'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_AND	  = r'and'
t_OR	  = r'or'
t_NOT	  = r'not'

def t_NUMBER(t):
	r'\d+'
	t.value = int(t.value)
	return t

# Precedence
precedence = ()	

# Yacc Grammar
def p_expression(p):
	'''expression : GREATER
				  | AND'''
	print(p[1])
	
def p_error(p):
	print("Syntax error at '%s'" % p.value)

lex.lex()
yacc.yacc()

while True:
	try:
		s = input('>>> ')
	except EOFError:
		break
	yacc.parse(s)