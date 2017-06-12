# -----------------------------------------------------------------------------
# mli.py
#
# A miniLisp interpreter.
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc

reserved = {
	'mod' : 'MOD',
	'and' : 'AND',
	'or' : 'OR',
	'not' : 'NOT',
	'define' : 'DEFINE',
	'fun' : 'FUN',
	'if' : 'IF',
	'print-num' : 'print_num'
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
t_TRUE    = r'\#t'
t_FALSE   = r'\#f'

t_ignore = " \t"

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
def p_program(p):
	'program : stmt'
	print("success !!")

def p_stmt_1(p):
	'stmt : exp'

def p_stmt_2(p):
	'stmt : print_stmt'

def p_print_stmt_1(p):
	'print_stmt : LPAREN print_num exp RPAREN'

def p_exp_1(p):
	'exp : num_op'

def p_num_op_1(p):
	'num_op : exp_plus'

def p_expression_plus(p):
	'exp_plus : LPAREN PLUS exp exps RPAREN'
	if(isinstance(p[4], list)):
		for i in p[4]:
			p[3] = p[3] + i
		print(p[3])
	elif(isinstance(p[4], int)):
		print(p[3] + p[4])

def p_expression_more_1(p):
	'exps : exps exp'
	if isinstance(p[1], list):
		p[1].append(p[2])
		p[0] = p[1]
		print(p[0])
	elif isinstance(p[1], int):
		p[0] = []
		p[0].append(p[1])
		p[0].append(p[2])
		print(p[0])
def p_expression_more_2(p):
	'exps : exp'
	p[0] = p[1]
	print(p[0])

def p_expression(p):
	'exp : NUMBER'
	p[0] = p[1]

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