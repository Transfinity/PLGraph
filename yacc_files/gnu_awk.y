/*
 * awkgram.y --- yacc/bison parser
 */

/* 
 * Copyright (C) 1986, 1988, 1989, 1991-2012 the Free Software Foundation, Inc.
 * 
 * This file is part of GAWK, the GNU implementation of the
 * AWK Programming Language.
 * 
 * GAWK is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * GAWK is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

%{
%}

%token FUNC_CALL NAME REGEXP FILENAME
%token YNUMBER YSTRING
%token RELOP IO_OUT IO_IN
%token ASSIGNOP ASSIGN MATCHOP CONCAT_OP
%token SUBSCRIPT
%token LEX_BEGIN LEX_END LEX_IF LEX_ELSE LEX_RETURN LEX_DELETE
%token LEX_SWITCH LEX_CASE LEX_DEFAULT LEX_WHILE LEX_DO LEX_FOR LEX_BREAK LEX_CONTINUE
%token LEX_PRINT LEX_PRINTF LEX_NEXT LEX_EXIT LEX_FUNCTION
%token LEX_BEGINFILE LEX_ENDFILE 
%token LEX_GETLINE LEX_NEXTFILE
%token LEX_IN
%token LEX_AND LEX_OR INCREMENT DECREMENT
%token LEX_BUILTIN LEX_LENGTH
%token LEX_EOF
%token LEX_INCLUDE LEX_EVAL
%token NEWLINE

/* Lowest to highest */
%right ASSIGNOP ASSIGN SLASH_BEFORE_EQUAL
%right '?' ':'
%left LEX_OR
%left LEX_AND
%left LEX_GETLINE
%nonassoc LEX_IN
%left FUNC_CALL LEX_BUILTIN LEX_LENGTH
%nonassoc ','
%left MATCHOP
%nonassoc RELOP '<' '>' IO_IN IO_OUT
%left CONCAT_OP
%left YSTRING YNUMBER
%left '+' '-'
%left '*' '/' '%'
%right '!' UNARY
%right '^'
%left INCREMENT DECREMENT
%left '$'
%left '(' ')'
%%

program
	: /* empty */
	| program rule
	  
	| program nls
	| program LEX_EOF
	  
	| program error
	  
	;

rule
	: pattern action
	  
	| pattern statement_term
	  
	| function_prologue action
	  
	| '@' LEX_INCLUDE source statement_term
	  
	;

source
	: FILENAME
	  
	| FILENAME error
	  
	| error
	  
	;

pattern
	: /* empty */
	  
	| exp
	  
	| exp ',' opt_nls exp
	  
	| LEX_BEGIN
	  
	| LEX_END
	  
	| LEX_BEGINFILE
	  
	| LEX_ENDFILE
	  
	;

action
	: l_brace statements r_brace opt_semi opt_nls
	  
	;

func_name
	: NAME
	  
	| FUNC_CALL
	  
	| lex_builtin
	  
	| '@' LEX_EVAL
	  
	;

lex_builtin
	: LEX_BUILTIN
	| LEX_LENGTH
	;
		
function_prologue
	: LEX_FUNCTION
	  
		func_name '(' opt_param_list r_paren opt_nls
	 	
	;

regexp
	/*
	 * In this rule, want_regexp tells yylex that the next thing
	 * is a regexp so it should read up to the closing slash.
	 */
	: a_slash
		
	  REGEXP	/* The terminating '/' is consumed by yylex(). */
		
	;

a_slash
	: '/'
	  
	| SLASH_BEFORE_EQUAL
	;

statements
	: /* empty */
	  
	| statements statement
	  
	| statements error
	  
	;

statement_term
	: nls
	| semi opt_nls
	;

statement
	: semi opt_nls
	  
	| l_brace statements r_brace
	  
	| if_statement
	  
	| LEX_SWITCH '(' exp r_paren opt_nls l_brace case_statements opt_nls r_brace
	  
	| LEX_WHILE '(' exp r_paren opt_nls statement
	  
	| LEX_DO opt_nls statement LEX_WHILE '(' exp r_paren opt_nls
	  
	| LEX_FOR '(' NAME LEX_IN simple_variable r_paren opt_nls statement
	  
	| LEX_FOR '(' opt_simple_stmt semi opt_nls exp semi opt_nls opt_simple_stmt r_paren opt_nls statement
	  
	| LEX_FOR '(' opt_simple_stmt semi opt_nls semi opt_nls opt_simple_stmt r_paren opt_nls statement
	  
	| non_compound_stmt
	  
	;

non_compound_stmt
	: LEX_BREAK statement_term
	  
	| LEX_CONTINUE statement_term
	  
	| LEX_NEXT statement_term
	  
	| LEX_NEXTFILE statement_term
	  
	| LEX_EXIT opt_exp statement_term
	  
	| LEX_RETURN
	   opt_exp statement_term 
	| simple_stmt statement_term
	;

	/*
	 * A simple_stmt exists to satisfy a constraint in the POSIX
	 * grammar allowing them to occur as the 1st and 3rd parts
	 * in a `for (...;...;...)' loop.  This is a historical oddity
	 * inherited from Unix awk, not at all documented in the AK&W
	 * awk book.  We support it, as this was reported as a bug.
	 * We don't bother to document it though. So there.
	 */
simple_stmt
	: print  print_expression_list output_redir
	  

	| LEX_DELETE NAME  delete_subscript_list
	  	
	| LEX_DELETE '(' NAME ')'
		  /*
		   * this is for tawk compatibility. maybe the warnings
		   * should always be done.
		   */
	  
	| exp
	  
	;

opt_simple_stmt
	: /* empty */
	  
	| simple_stmt
	  
	;

case_statements
	: /* empty */
	  
	| case_statements case_statement
	  
	| case_statements error
	  
	;

case_statement
	: LEX_CASE case_value colon opt_nls statements
	  
	| LEX_DEFAULT colon opt_nls statements
	  
	;

case_value
	: YNUMBER
	  
	| '-' YNUMBER    %prec UNARY
	  
	| '+' YNUMBER    %prec UNARY
	  
	| YSTRING 
	  
	| regexp  
	  
	;

print
	: LEX_PRINT
	  
	| LEX_PRINTF
	  
	;

	/*
	 * Note: ``print(x)'' is already parsed by the first rule,
	 * so there is no good in covering it by the second one too.
	 */
print_expression_list
	: opt_expression_list
	| '(' expression_list r_paren
	  
	;

output_redir
	: /* empty */
	  
	| IO_OUT  common_exp
	  
	;

if_statement
	: LEX_IF '(' exp r_paren opt_nls statement
	  
	| LEX_IF '(' exp r_paren opt_nls statement
	     LEX_ELSE opt_nls statement
	  
	;

nls
	: NEWLINE
	| nls NEWLINE
	;

opt_nls
	: /* empty */
	| nls
	;

input_redir
	: /* empty */
	  
	| '<' simp_exp
	  
	;

opt_param_list
	: /* empty */
	| param_list
	;

param_list
	: NAME
	  
	| param_list comma NAME
	  
	| error
	  
	| param_list error
	  
	| param_list comma error
	  
	;

/* optional expression, as in for loop */
opt_exp
	: /* empty */
	  
	| exp
	  
	;

opt_expression_list
	: /* empty */
	  
	| expression_list
	  
	;

expression_list
	: exp
	  
	| expression_list comma exp
	  
	| error
	  
	| expression_list error
	  
	| expression_list error exp
	  
	| expression_list comma error
	  
	;

/* Expressions, not including the comma operator.  */
exp
	: variable assign_operator exp %prec ASSIGNOP
	  
	| exp LEX_AND exp
	  
	| exp LEX_OR exp
	  
	| exp MATCHOP exp
	  
	| exp LEX_IN simple_variable
	  
	| exp a_relop exp %prec RELOP
	  
	| exp '?' exp ':' exp
	  
	| common_exp
	  
	;

assign_operator
	: ASSIGN
	  
	| ASSIGNOP
	  
	| SLASH_BEFORE_EQUAL ASSIGN   /* `/=' */
	  
	;

relop_or_less
	: RELOP
	  
	| '<'
	  
	;

a_relop
	: relop_or_less
	  
	| '>'
	  
	;

common_exp
	: simp_exp
	  
	| simp_exp_nc
	  
	| common_exp simp_exp %prec CONCAT_OP
	  
	;

simp_exp
	: non_post_simp_exp
	/* Binary operators in order of decreasing precedence.  */
	| simp_exp '^' simp_exp
	  
	| simp_exp '*' simp_exp
	  
	| simp_exp '/' simp_exp
	  
	| simp_exp '%' simp_exp
	  
	| simp_exp '+' simp_exp
	  
	| simp_exp '-' simp_exp
	  
	| LEX_GETLINE opt_variable input_redir
	  
	| variable INCREMENT
	  
	| variable DECREMENT
	  
	| '(' expression_list r_paren LEX_IN simple_variable
	  
	;

/* Expressions containing "| getline" lose the ability to be on the
   right-hand side of a concatenation. */
simp_exp_nc
	: common_exp IO_IN LEX_GETLINE opt_variable
		
	/* Binary operators in order of decreasing precedence.  */
	| simp_exp_nc '^' simp_exp
	  
	| simp_exp_nc '*' simp_exp
	  
	| simp_exp_nc '/' simp_exp
	  
	| simp_exp_nc '%' simp_exp
	  
	| simp_exp_nc '+' simp_exp
	  
	| simp_exp_nc '-' simp_exp
	  
	;

non_post_simp_exp
	: regexp
	  
	| '!' simp_exp %prec UNARY
	  
	| '(' exp r_paren
	  
	| LEX_BUILTIN '(' opt_expression_list r_paren
	  
	| LEX_LENGTH '(' opt_expression_list r_paren
	  
	| LEX_LENGTH
	  
	| func_call
	| variable
	| INCREMENT variable
	  
	| DECREMENT variable
	  
	| YNUMBER
	  
	| YSTRING
	  
	| '-' simp_exp    %prec UNARY
	  
	| '+' simp_exp    %prec UNARY
	  
	;

func_call
	: direct_func_call
	  
	| '@' direct_func_call
	  
	;

direct_func_call
	: FUNC_CALL '(' opt_expression_list r_paren
	  
	;

opt_variable
	: /* empty */
	  
	| variable
	  
	;

delete_subscript_list
	: /* empty */
	  
	| delete_subscript SUBSCRIPT
	  
	;

delete_subscript
	: delete_exp_list
	  
	| delete_subscript delete_exp_list
	  
	;

delete_exp_list
	: bracketed_exp_list
	  
	;

bracketed_exp_list
	: '[' expression_list ']'
  	  
	;

subscript
	: bracketed_exp_list
	  
	| subscript bracketed_exp_list
	  
	;

subscript_list
	: subscript SUBSCRIPT
	  
	;

simple_variable
	: NAME
	  
	| NAME subscript_list
	  
	;

variable
	: simple_variable
	| '$' non_post_simp_exp opt_incdec
	  
	;

opt_incdec
	: INCREMENT
	  
	| DECREMENT
	  
	| /* empty */	
	;

l_brace
	: '{' opt_nls
	;

r_brace
	: '}' opt_nls	
	;

r_paren
	: ')' 
	;

opt_semi
	: /* empty */
	| semi
	;

semi
	: ';'	
	;

colon
	: ':'	
	;

comma
	: ',' opt_nls	
	;
%%
