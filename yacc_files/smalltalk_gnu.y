/******************************** -*- C -*- ****************************
 *
 *	GNU Smalltalk language grammar definition
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 1988,89,90,91,92,94,95,99,2000,2001,2002,2003
 * Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
 *
 ***********************************************************************/

%{
%}

%name-prefix="_gst_yy"
%debug
%pure_parser

/* single definite characters */     
%token PRIMITIVE_START "<primitive: ...>"
%token INTERNAL_TOKEN
%token SCOPE_SEPARATOR "'.' or '::'"
%token ASSIGNMENT "'_' or ':='"

/* larger lexical items */
%token <sval> IDENTIFIER "identifier"
%token <sval> KEYWORD "keyword message"
%token <sval> STRING_LITERAL "string literal"
%token <sval> SYMBOL_KEYWORD "symbol literal"
%token <sval> BINOP "binary message"
%token <sval> '|'
%token <ival> INTEGER_LITERAL "integer literal"
%token <ival> BYTE_LITERAL "integer literal"
%token <fval> FLOATD_LITERAL "floating-point literal"
%token <fval> FLOATE_LITERAL "floating-point literal"
%token <fval> FLOATQ_LITERAL "floating-point literal"
%token <cval> CHAR_LITERAL "character literal"
%token <oval> SCALED_DECIMAL_LITERAL "scaled decimal literal"
%token <boval> LARGE_INTEGER_LITERAL "integer literal"

%%

program:
	internal_marker method
	| file_in
	| /* empty */
	;

internal_marker:
	INTERNAL_TOKEN
		
	;

file_in:
	doit_and_method_list
	| file_in doit_and_method_list
	;

doit_and_method_list:
	doit
	| doit internal_marker method_list '!'
		

	;

doit:
	temporaries statements '!'

		
	| error '!'
		
	;

method_list:
        method '!' method_list
		
	| error '!'
		
	| /* EMPTY */
	;
     
method:
	message_pattern temporaries primitive statements
		
	;

message_pattern:
	unary_selector
		
	| binary_selector variable_name
		
	| keyword_variable_list
		
	;

unary_selector:
	IDENTIFIER
	;

binary_selector:
	BINOP
	| '|'
	;

variable_name:
	IDENTIFIER
		
	;

keyword_variable_list:
	keyword variable_name
		
	| keyword_variable_list keyword variable_name
		
	;

keyword:
	KEYWORD
	;

primitive:
	/* empty */
		
	| PRIMITIVE_START IDENTIFIER primitive_end
		
	;

primitive_end: BINOP
		
	;

temporaries:
	/* empty */
		
	| '|' '|'
		
	| '|' variable_names '|'
		
	;

variable_names:
	variable_name
		
	| variable_names variable_name
		
	;

statements:
	/* empty */
		
	| return_statement optional_dot	
		
	| statements.1 optional_dot
		
	| statements.1 '.' return_statement optional_dot	
		
	;

statements.1:
	statement
		
	| statements.1 '.' statement
		
	| statements.1 '.' error
		
	;
	
optional_dot:
	/* empty */
 	| '.'
	;

statement:
	expression
		
	;

return_statement:
	'^' expression 
		
	;

expression:
	simple_expression
	| assigns simple_expression
		
	;

assigns:
	variable_primary ASSIGNMENT
		
	| assigns variable_primary ASSIGNMENT
		
	;

simple_expression:
	primary
	| message_expression
	| cascaded_message_expression
	;

primary:
	variable_primary
	| literal
	| block				
	| array_constructor
	| '(' error '!'
		
	| '(' error ')'
		
	| '(' expression ')'
		
	;

variable_primary:
	IDENTIFIER
		
	| variable_primary SCOPE_SEPARATOR IDENTIFIER
		
	;

literal:
	number
	| small_number
	| symbol_constant
	| character_constant
	| string
	| array_constant
	| variable_binding
	| compile_time_constant
	;

number:
	INTEGER_LITERAL
		
	| FLOATD_LITERAL
		
	| FLOATE_LITERAL
		
	| FLOATQ_LITERAL
		
	| LARGE_INTEGER_LITERAL
		
	| SCALED_DECIMAL_LITERAL
		
	;

small_number:
	BYTE_LITERAL
		
	;

symbol_constant:
	'#' symbol
		
	| '#' STRING_LITERAL
		
	;

symbol:
	IDENTIFIER
		
	| binary_selector
		
	| SYMBOL_KEYWORD
		
	| KEYWORD
		
	;


character_constant:
	CHAR_LITERAL
		
	;

string:
	STRING_LITERAL
		
	;

array_constant:
	'#' array
		
	| '#' byte_array
		
	;

array:
	'(' ')'
		
	| '(' error '!'
		
	| '(' error ')'
		
	| '(' array_constant_list ')'
		
	;

array_constant_list:
	array_constant_elt
		
	| array_constant_list array_constant_elt
		
	;

array_constant_elt:
	array
	| byte_array
	| literal
	| symbol
		
	;

byte_array:
	'[' ']'
		
	| '[' error '!'
		
	| '[' error ']'
		
	| '[' byte_array_constant_list ']'
		
	;

byte_array_constant_list:
	small_number
		
	| byte_array_constant_list small_number
		
	;

variable_binding:
	'#' '{' error '!'
		
	| '#' '{' error '}'
		
	| '#' '{' variable_primary '}'

		
	;

compile_time_constant:
	'#' '#' compile_time_constant_body
		
	;

compile_time_constant_body:
	'(' error '!'
		
	| '(' error ')'
		
	| '(' temporaries statements ')'
		
	;

array_constructor:
	'{' error '!'
		
	| '{' error '}'
		
	| '{' statements '}'
		
	;

block:
	'[' error '!'
		
	| '[' error ']'
		
	| '[' opt_block_variables temporaries statements ']'
		
	;

opt_block_variables:
	/* empty */
		
	| block_variable_list binary_selector
		
	;

/* syntax for blocks with temporaries is just args and vertical bar (if
 * any followed by a standard temporaries declarations */

block_variable_list:
	':' variable_name
		
	| block_variable_list ':' variable_name
		
	;

message_expression:
	unary_expression
	| binary_expression
	| keyword_expression
	;

unary_expression:
	unary_object_description unary_selector
		
	;

unary_object_description:
	primary
	| unary_expression
	;

binary_expression:
	binary_object_description binary_selector unary_object_description
		
	;

binary_object_description:
	unary_object_description
	| binary_expression
	;

keyword_expression:
	binary_object_description keyword_binary_object_description_list
		
 	;

keyword_binary_object_description_list:
	keyword binary_object_description
		
	| keyword_binary_object_description_list keyword
	  binary_object_description
		
	;

cascaded_message_expression:
	message_expression semi_message_list
		
	;

semi_message_list:
	';' message_elt
		
	| semi_message_list ';' message_elt
		
	;

message_elt:
	unary_selector
		
	| binary_selector unary_object_description
		
	| keyword_binary_object_description_list
		
	;
