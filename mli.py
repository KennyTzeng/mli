# -----------------------------------------------------------------------------
# mli.py
#
# A miniLisp interpreter.
# -----------------------------------------------------------------------------\

import ply.lex as lex
import ply.yacc as yacc

reserved = {
	'mod' : 'MOD',
	'and' : 'AND',
	'or' : 'OR',
	'not' : 'NOT',
	'define' : 'DEFINE',
	'fun' : 'FUN',
	'if' : 'IF'
}

tokens = [
			'ID','NUMBER','TRUE','FALSE',
			'PLUS','MINUS','TIMES','DIVIDE',
			'GREATER','SMALLER','EQUALS','LPAREN','RPAREN',
		] + list(reserved.values())

# Lex Tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
# t_MOD	  = r'mod'
t_GREATER = r'>'
t_SMALLER = r'<'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
# t_AND	  = r'and'
# t_OR	  = r'or'
# t_NOT	  = r'not'
t_TRUE    = r'#t'
t_FALSE   = r'#f'

def t_ID(t):
	r'[a-z]([a-z]|\d|-)*'
	t.type = reserved.get(t.value, 'ID')
	print(t.type)
	return t

def t_NUMBER(t):
	r'0|[1-9]\d*|-[1-9]\d*'
	t.value = int(t.value)
	print(t.type)
	return t

def t_error(t):
	print("lex error")
	t.lexer.skip(1)

# Precedence
precedence = ()

# Yacc Grammar
def p_expression(p):
	'''expression : GREATER
				  | AND'''
	print(p[1])
	
def p_error(p):
	# print("Syntax error at '%s'" % p.value)
	print("yacc error")

lex.lex()
yacc.yacc()

while True:
	try:
		s = input('>>> ')
	except EOFError:
		break
	yacc.parse(s)