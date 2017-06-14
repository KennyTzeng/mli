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
	'print-num' : 'print_num',
	'print-bool' : 'print_bool'
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
	#print(t.type)
	return t

def t_NUMBER(t):
	r'0|[1-9]\d*|-[1-9]\d*'
	t.value = int(t.value)
	#print(t.type)
	return t

def t_error(t):
	print("lex error")
	t.lexer.skip(1)

# Precedence
precedence = ()

# variables
vars = {}

# Yacc Grammar
def p_program(p):
	'program : stmts'
	print("success !!")

def p_stmts(p):
	'''stmts : stmts stmt
			 | stmt'''

#STMT
def p_stmt_exp(p):
	'stmt : exp'

def p_stmt_print_stmt(p):
	'stmt : print_stmt'

def p_stmt_def_stmt(p):
	'stmt : def_stmt'

#PRINT-STMT
def p_print_stmt_print_num(p):
	'print_stmt : LPAREN print_num exp RPAREN'
	if(isinstance(p[3], int)):
		print(p[3])
	elif(isinstance(p[3], bool)):
		print("print-num can't print boolean value !")

def p_print_stmt_print_bool(p):
	'print_stmt : LPAREN print_bool exp RPAREN'
	if(isinstance(p[3], bool)):
		if(p[3]):
			print("#t")
		else:
			print("#f")
	elif(isinstance(p[3], int)):
		print("print-bool can't print integer value !")

#EXP
def p_exp_number(p):
	'exp : NUMBER'
	p[0] = p[1]

def p_exp_boolean_true(p):
	'exp : TRUE'
	p[0] = True

def p_exp_boolean_false(p):
	'exp : FALSE'
	p[0] = False

def p_exp_number_operation(p):
	'exp : num_op'
	p[0] = p[1]

def p_exp_logical_operation(p):
	'exp : logical_op'
	p[0] = p[1]

def p_exp_if_exp(p):
	'exp : if_exp'
	p[0] = p[1]

def p_exp_variable(p):
	'exp : variable'
	p[0] = vars.get(p[1])

#/EXP --------------------------------------------------

#NUM-OP
def p_num_op(p):
	'''num_op : exp_plus
			  |	exp_minus
			  | exp_times
			  | exp_divide
			  | exp_mod
			  | exp_greater
			  | exp_smaller
			  | exp_equals'''
	p[0] = p[1]

#EXP-PLUS
def p_exp_plus(p):
	'exp_plus : LPAREN PLUS exp exps RPAREN'
	if(isinstance(p[4], list)):
		for i in p[4]:
			p[3] = p[3] + i
		p[0] = p[3]
	elif(isinstance(p[4], int)):
		p[0] = p[3] + p[4]

#EXP-MINUS
def p_exp_minus(p):
	'exp_minus : LPAREN MINUS exp exp RPAREN'
	p[0] = p[3] - p[4]

#EXP-TIMES
def p_exp_times(p):
	'exp_times : LPAREN TIMES exp exps RPAREN'
	if(isinstance(p[4], list)):
		for i in p[4]:
			p[3] = p[3] * i
		p[0] = p[3]
	elif(isinstance(p[4], int)):
		p[0] = p[3] * p[4]

#EXP-DIVIDE
def p_exp_divide(p):
	'exp_divide : LPAREN DIVIDE exp exp RPAREN'
	p[0] = p[3] / p[4]

#EXP-MOD
def p_exp_mod(p):
	'exp_mod : LPAREN MOD exp exp RPAREN'
	p[0] = p[3] % p[4]

#EXP-GREATER
def p_exp_greater(p):
	'exp_greater : LPAREN GREATER exp exp RPAREN'
	p[0] = p[3] > p[4]

#EXP-SMALLER
def p_exp_smaller(p):
	'exp_smaller : LPAREN SMALLER exp exp RPAREN'
	p[0] = p[3] < p[4]

#EXP-EQUALS
def p_exp_equals(p):
	'exp_equals : LPAREN EQUALS exp exps RPAREN'
	if(isinstance(p[4], list)):
		p[4].sort()
		p[0] = ( p[3] == p[4][0] and p[3] == p[4][-1] )
	elif(isinstance(p[4], int)):
		p[0] = p[3] == p[4]

#/NUM-OP -----------------------------------------------

#LOGICAL-OP
def p_logical_op(p):
	'''logical_op : and_op
				  | or_op
				  | not_op'''
	p[0] = p[1]

#AND-OP
def p_and_op(p):
	'and_op : LPAREN AND exp exps RPAREN'
	if(isinstance(p[4], list)):
		p[4].sort()
		p[0] = p[3] and p[4][0] and p[4][-1]
	elif(isinstance(p[4], bool)):
		p[0] = p[3] and p[4]

#OR-OP
def p_or_op(p):
	'or_op : LPAREN OR exp exps RPAREN'
	if(isinstance(p[4], list)):
		p[4].sort()
		p[0] = p[3] or p[4][0] or p[4][-1]
	elif(isinstance(p[4], bool)):
		p[0] = p[3] or p[4]

#NOT-OP
def p_not_op(p):
	'not_op : LPAREN NOT exp RPAREN'
	p[0] = not p[3]

#/LOGICAL-OP -------------------------------------------

#IF-EXP
def p_if_exp(p):
	'if_exp : LPAREN IF test_exp than_exp else_exp RPAREN'
	if(p[3]):
		p[0] = p[4]
	else:
		p[0] = p[5]

def p_test_exp(p):
	'test_exp : exp'
	p[0] = p[1]

def p_than_exp(p):
	'than_exp : exp'
	p[0] = p[1]

def p_else_exp(p):
	'else_exp : exp'
	p[0] = p[1]

#/IF-EXP -----------------------------------------------

#DEFINE-STMT
def p_def_stmt(p):
	'def_stmt : LPAREN DEFINE variable exp RPAREN'
	if(vars.get(p[3]) == None):
		vars[p[3]] = p[4]

def p_variable_id(p):
	'variable : ID'
	p[0] = p[1]

#/DEFINE-STMT ------------------------------------------

#IDS
def p_ids_1(p):
	'IDs : IDs ID'
	if(isinstance(p[1], list)):
		p[0] = p[1]
		p[0].append(p[2])
	elif(isinstance(p[1], str)):
		p[0] = []
		p[0].append(p[1])
		p[0].append(p[2])

def p_ids_2(p):
	'IDs : ID'
	p[0] = p[1]

#/IDS --------------------------------------------------

#FUN-EXP
def p_fun_exp(p):
	'fun_exp : LPAREN FUN fun_ids fun_body RPAREN'

def p_fun_ids(p):
	'fun_ids : LPAREN IDs RPAREN'

def p_fun_body(p):
	'fun_body : exp'

#/FUN-EXP ----------------------------------------------
#FUN-CALL
def p_fun_call_exp(p):
	'fun_call : LPAREN fun_exp params RPAREN'

def p_fun_call_name(p):
	'fun_call : LPAREN fun_name params RPAREN'

def p_fun_name(p):
	'fun_name : ID'
	p[0] = p[1]

def p_param(p):
	'param : exp'
	p[0] = p[1]

def p_params_1(p):
	'params : params param'
	if(isinstance(p[1], list)):
		p[0] = p[1]
		p[0].append(p[2])
	else:
		p[0] = []
		p[0].append(p[1])
		p[0].append(p[2])

def p_params_2(p):
	'params : param'
	p[0] = p[1]

#/FUN-CALL ---------------------------------------------

#EXPS
def p_exps_1(p):
	'exps : exps exp'
	if isinstance(p[1], list):
		p[0] = p[1]
		p[0].append(p[2])
	elif isinstance(p[1], int) or isinstance(p[1], bool):
		p[0] = []
		p[0].append(p[1])
		p[0].append(p[2])

def p_exps_2(p):
	'exps : exp'
	p[0] = p[1]

#Yacc error
def p_error(p):
	# print("Syntax error at '%s'" % p.value)
	print("yacc error")

#
lex.lex()
yacc.yacc()

while True:
	try:
		s = input('>>> ')
	except EOFError:
		break
	yacc.parse(s)