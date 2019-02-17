#!/usr/bin/env python3

import sys

tokens = [
    'NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','EQUALS',
    'LPAREN','RPAREN','NAME','COMMA','SEMICOLON',
    'LBRACKET','RBRACKET','COLON','HASH','newline',
    'IGNOREPARENCONTENT', 'RBRACE', 'NONSPACEIG',
    'DOUBLECOLON', 'LCURLY', 'RCURLY', 'TICK',
    'IGNOREBEGINENDCONTENT', 'AT', 'POINT',
    'NUMBERSPEC', 'CONDOR', 'CONDAND', 'LOGICOR',
    'LOGICAND', 'INVERT', 'LOGICINVERT' , 'XOR',
    'EQUALCOND', 'UNEQUALCOND'
    ]
reserved = {
  'module' : 'MODULE',
  'endmodule' : 'ENDMODULE',
  'logic' : 'LOGIC',
  'wire'  : 'WIRE',
  'reg'   : 'REG',
  'input'   : 'INPUT',
  'output'  : 'OUTPUT',
  'inout'   : 'INOUT',
  'parameter'   : 'PARAMETER',
  'int'        : 'INT',
  'always_comb': 'ALWAYS_COMB',
  'begin'      : 'BEGIN',
  'end'        : 'END',
  'always_ff'  : 'ALWAYS_FF',
  'or'         : 'OR',
  'posedge'    : 'POSEDGE',
  'negedge'    : 'NEGEDGE',
  'initial'    : 'INITIAL',
  'forever'    : 'FOREVER',
  'always_latch': 'ALWAYS_LATCH',
  'localparam' : 'LOCALPARAM',
  'assign'     : 'ASSIGN',
  'generate'   : 'GENERATE',
  'endgenerate'   : 'ENDGENERATE',
  'if'         : 'IF',
  'else'       : 'ELSE',
  'for'        : 'FOR',
}

states = (
    ('ignoreparen','exclusive'),
    ('ignorebeginend','exclusive'),
)

tokens += reserved.values()
# Build the lexer
import ply.lex as lex

# Tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LCURLY  = r'{'
t_RCURLY  = r'}'
t_TICK    = r'\''
t_AT      = r'@'
t_XOR     = r'\^'
t_INVERT  = r'!'
t_LOGICINVERT  = r'~'
t_LBRACKET  = r'\['
t_RBRACKET  = r'\]'
t_HASH    = r'\#'
t_COMMA   = r','
t_POINT   = r'\.'
t_SEMICOLON = r';'
t_MODULE  = r'module'
t_ENDMODULE  = r'endmodule'

lineno = 1

def t_EQUALCOND(t):
    r'=='
    return t

def t_EQUALS(t):
    r'='
    return t

def t_UNEQUALCOND(t):
    r'!='
    return t

def t_DOUBLECOLON(t):
    r'::'
    return t

def t_COLON(t):
    r':'
    return t

def t_CONDOR(t):
    r'\|\|'
    return t

def t_LOGICOR(t):
    r'\|'
    return t

def t_CONDAND(t):
    r'&&'
    return t

def t_LOGICAND(t):
    r'&'
    return t

def t_NAME(t):
    r'[a-zA-Z_\$][a-zA-Z0-9_]*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_NUMBERSPEC(t):
    r'\d+\'[bhd][\dxz]+'
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


# Rules for the ignoreparen state
def t_ignoreparen_lbrace(t):     
    r'\('
    t.lexer.level +=1                

def t_ignoreparen_RBRACE(t):
    r'\)'
    global lineno
    t.lexer.level -=1

    # If closing brace, return the code fragment
    if t.lexer.level == 0:
         t.value = t.lexer.lexdata[t.lexer.ignore_start:t.lexer.lexpos-1]
         print(t.value)
         t.type = "IGNOREPARENCONTENT"
         t.lexer.lineno += t.value.count('\n')
         lineno += t.value.count('\n')
         t.lexer.begin('INITIAL')           
         return t

def t_ignoreparen_NONSPACEIG(t):
   r'[^\(\)]+'

# Ignored characters (whitespace)
t_ignoreparen_ignore = " \t\n"


# For bad characters, we just skip over it
def t_ignoreparen_error(t):
    t.lexer.skip(1)



# Rules for the ignorebeginend state
def t_ignorebeginend_BEGIN(t):     
    r'begin'
    t.lexer.level +=1                
    t.lexer.isbeginend = 1

def t_ignorebeginend_END(t):
    r'end'
    global lineno
    t.lexer.level -=1

    # If closing brace, return the code fragment
    if t.lexer.level == 0:
         t.value = t.lexer.lexdata[t.lexer.ignore_start:t.lexer.lexpos]
         #print(t.value)
         t.type = "IGNOREBEGINENDCONTENT"
         t.lexer.lineno += t.value.count('\n')
         lineno += t.value.count('\n')
         t.lexer.begin('INITIAL')           
         return t

def t_ignorebeginend_NONSPACEIG(t):
   r'.'
   global lineno
   if not t.lexer.isbeginend and t.value == ";":
     t.value = t.lexer.lexdata[t.lexer.ignore_start:t.lexer.lexpos]
     #print(t.value)
     t.type = "IGNOREBEGINENDCONTENT"
     t.lexer.lineno += t.value.count('\n')
     lineno += t.value.count('\n')
     t.lexer.begin('INITIAL')           
     return t

# Ignored characters (whitespace)
t_ignorebeginend_ignore = " \t\n"

# For bad characters, we just skip over it
def t_ignorebeginend_error(t):
    t.lexer.skip(1)



# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    global lineno
    t.lexer.lineno += t.value.count("\n")
    lineno += t.value.count("\n")
    print("New lines: %d" % t.lexer.lineno)

def t_error(t):
    print("Illegal character on line '%d'" % t.lexer.lineno)
    #print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


global_lexer = lex.lex()

global_lexer.lineno = 1

# Precedence rules for the arithmetic operators
precedence = (
    ('nonassoc','EQUALCOND','UNEQUALCOND'),
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE'),
    ('right','UNARY_OPS'),
    )

# dictionary of names (for storing variables)
names = { }

def p_statement(p):
    '''statement : 
                  | statement line 
    '''
def p_line(p):
    '''line : newline
            | module
    '''

def p_module(p):
    '''module : MODULE NAME param_ports_opt module_ports_opt SEMICOLON module_body ENDMODULE
    '''

def p_module_ports_opt(p):
    '''module_ports_opt : LPAREN RPAREN
                         | 
                         | LPAREN module_ports RPAREN
    '''
                         #  | start_ignore_paren IGNOREPARENCONTENT

def p_start_ignore_parent(p):
    '''start_ignore_paren : LPAREN'''
    global_lexer.begin("ignoreparen")
    global_lexer.ignore_start = global_lexer.lexpos        # Record the starting position
    global_lexer.level = 1                          # Initial brace level

def p_module_ports(p):
    '''module_ports : module_port
                     | module_ports COMMA module_port
    '''

def p_module_port(p):
    '''module_port : portdir_opt port_data_type ranges_opt NAME ranges_opt
    '''

def p_port_data_type(p):
    '''port_data_type :  datatype
                        | customport
    '''

def p_portdir(p):
    '''portdir : INPUT
                 | OUTPUT
                 | INOUT
    '''

def p_customport(p):
    '''customport : NAME POINT NAME
                    | NAME
    '''

def p_portdir_opt(p):
    '''portdir_opt : portdir
                     | '''

def p_param_ports_opt(p):
    '''param_ports_opt : 
                        | HASH LPAREN RPAREN
                        | HASH LPAREN param_ports RPAREN
    '''

def p_param_ports(p):
    '''param_ports : param_port
                     | param_ports COMMA param_port
    '''

def p_param_port(p):
    '''param_port : PARAMETER paramtype_opt ranges_opt NAME ranges_opt assignment_opt
    '''
    print(p[4]+" on line number %d" % lineno)

def p_paramtype_opt(p):
    '''paramtype_opt : paramtype
                      | 
    '''

def p_paramtype(p):
    '''paramtype : INT
    '''

def p_module_body(p):
    '''module_body :  module_body module_body_line
                    |
    '''

def p_module_body_line(p):
    '''module_body_line : datatype ranges_opt NAME ranges_opt assignment_opt SEMICOLON
                        | ignore_block IGNOREBEGINENDCONTENT
                        | LOCALPARAM NAME assignment_opt SEMICOLON
                        | instance
                        | generate_block
    '''

def p_generate_block(p):
    '''generate_block : GENERATE generate_body ENDGENERATE
    '''

def p_generate_body(p):
    '''generate_body :  generate_body generate_body_line
                    |
    '''

def p_generate_body_line(p):
    '''generate_body_line :  if_block
    '''

def p_if_block(p):
    '''if_block : IF LPAREN expression_opt RPAREN BEGIN module_body END
    '''

def p_ignore_block(p):
    '''ignore_block : ALWAYS_COMB
                     | always_ff_expr
                     | INITIAL
                     | ALWAYS_LATCH
                     | FOREVER
                     | ASSIGN
    '''
    global_lexer.begin("ignorebeginend")
    global_lexer.ignore_start = global_lexer.lexpos        # Record the starting position
    global_lexer.level = 0                          # Initial brace level
    global_lexer.isbeginend = 0

def p_instance(p):
    '''instance : NAME instanceparam_opt NAME instancesignal_opt SEMICOLON
    '''

def p_instanceparam_opt(p):
    '''instanceparam_opt : instanceparam
                          |
    '''

def p_instanceparam(p):
    '''instanceparam : HASH LPAREN connections_opt RPAREN
    '''

def p_instancesignal_opt(p):
    '''instancesignal_opt : instancesignal
                          |
    '''

def p_instancesignal(p):
    '''instancesignal : LPAREN connections_opt RPAREN
    '''

def p_connections_opt(p):
    '''connections_opt : connections
                        |
    '''

def p_connections(p):
    '''connections : connection
                    | connections COMMA connection
    '''

def p_connection(p):
    '''connection : POINT NAME LPAREN expressions_or_expressions_comma_opt RPAREN
    '''

def p_always_ff_expr(p):
    '''always_ff_expr : ALWAYS_FF AT LPAREN edges RPAREN
    '''

def p_edges(p):
    '''edges : edge
              | edges OR edge
    '''

def p_edge(p):
    '''edge :  POSEDGE NAME ranges_opt
             | NEGEDGE NAME ranges_opt
    '''

def p_datatype(p):
    '''datatype :  LOGIC
                 | REG
                 | WIRE
    '''

def p_assignment_opt(p):
    '''assignment_opt : assignment
                        | 
    '''

def p_assignment(p):
    '''assignment : EQUALS expression_or_expressions_comma
    '''

def p_expression_opt(p):
    '''expression_opt : expression
                        |
    '''
def p_expression_or_expressions_comma_opt(p):
    '''expressions_or_expressions_comma_opt : expression_or_expressions_comma
                                             |
    '''

def p_expression_or_expressions_comma(p):
    '''expression_or_expressions_comma :  optional_tick LCURLY expressions_comma_opt RCURLY
                                        | expression
    '''

def p_optional_tick(p):
    '''optional_tick : TICK
                      |
    '''

def p_expression(p):
    '''expression :   basic_expression
    '''
                    #| compareexpression
                    #| expression compareexpression

def p_basic_expression(p):
    '''basic_expression :    NUMBER
                           | name_or_name_in_package
                           | NUMBERSPEC
                           | LPAREN expression RPAREN
                           | basic_expression PLUS basic_expression
                           | basic_expression MINUS basic_expression
                           | basic_expression TIMES basic_expression
                           | basic_expression DIVIDE basic_expression
                           | basic_expression CONDOR basic_expression
                           | basic_expression CONDAND basic_expression
                           | basic_expression LOGICOR basic_expression
                           | basic_expression LOGICAND basic_expression
                           | basic_expression XOR basic_expression
                           | basic_expression EQUALCOND basic_expression
                           | INVERT basic_expression %prec UNARY_OPS
                           | LOGICINVERT basic_expression %prec UNARY_OPS
                           | MINUS basic_expression %prec UNARY_OPS
    '''
                                # | compareexpression
                                # | NUMBERSPEC
                                # | name_or_name_in_package
                                # | PLUS
                                # | MINUS
                                # | TIMES
                                # | DIVIDE
                                # | CONDOR
                                # | CONDAND
                                # | LOGICOR
                                # | LOGICAND
                                # | INVERT
                                # | LOGICINVERT
                                # | XOR


def p_expressions_comma_opt(p):
    '''expressions_comma_opt : expressions_comma
                               |
    '''

def p_expressions_comma(p):
    '''expressions_comma :  expression_or_expressions_comma
                           | expressions_comma COMMA expression_or_expressions_comma

    '''

def p_name_or_name_in_package(p):
    '''name_or_name_in_package :  NAME ranges_opt
                                | name_in_package ranges_opt
    '''

def p_name_in_package(p):
    '''name_in_package : NAME DOUBLECOLON NAME
    '''
    p[0] = "%s%s%s" % (p[1],p[2],p[3])

def p_ranges_opt(p):
    '''ranges_opt : ranges
                |
    '''

def p_ranges(p):
    '''ranges : range
                | ranges range
    '''

def p_range(p):
    '''range : LBRACKET range_numbers RBRACKET
    '''

def p_range_numbers(p):
    '''range_numbers : expression
                       | expression COLON expression
    '''


#def p_statement_assign(p):
#    'statement : NAME EQUALS NUMBER'
#    names[p[1]] = p[3]
#
#def p_statement_expr(p):
#    'statement : NUMBER'
#    print(p[1])

#def p_expression_binop(p):
#    '''expression : expression PLUS expression
#                  | expression MINUS expression
#                  | expression TIMES expression
#                  | expression DIVIDE expression'''
#    if p[2] == '+'  : p[0] = p[1] + p[3]
#    elif p[2] == '-': p[0] = p[1] - p[3]
#    elif p[2] == '*': p[0] = p[1] * p[3]
#    elif p[2] == '/': p[0] = p[1] / p[3]
#
#def p_expression_uminus(p):
#    'expression : MINUS expression %prec UMINUS'
#    p[0] = -p[2]
#
#def p_expression_group(p):
#    'expression : LPAREN expression RPAREN'
#    p[0] = p[2]
#
#def p_expression_number(p):
#    'expression : NUMBER'
#    p[0] = p[1]
#
#def p_expression_name(p):
#    'expression : NAME'
#    try:
#        p[0] = names[p[1]]
#    except LookupError:
#        print("Undefined name '%s'" % p[1])
#        p[0] = 0

def p_error(p):
    print("Syntax error at line '%d'" % lineno)
    sys.exit(-1)

import ply.yacc as yacc
yacc.yacc()

with open("inputfile.sv","r") as fp:
    filecontent = fp.read()
    yacc.parse(filecontent)

