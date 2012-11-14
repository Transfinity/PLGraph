/*

Copyright (C) 1993-2012 John W. Eaton
Copyright (C) 2009 David Grundberg
Copyright (C) 2009-2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Parser for Octave.

// C decarations.

%{
%}

// Bison declarations.

// Don't add spaces around the = here; it causes some versions of
// bison to fail to properly recognize the directive.

%name-prefix="octave_"

// Tokens with line and column information.
%token <tok_val> '=' ':' '-' '+' '*' '/'
%token <tok_val> ADD_EQ SUB_EQ MUL_EQ DIV_EQ LEFTDIV_EQ POW_EQ
%token <tok_val> EMUL_EQ EDIV_EQ ELEFTDIV_EQ EPOW_EQ AND_EQ OR_EQ
%token <tok_val> LSHIFT_EQ RSHIFT_EQ LSHIFT RSHIFT
%token <tok_val> EXPR_AND_AND EXPR_OR_OR
%token <tok_val> EXPR_AND EXPR_OR EXPR_NOT
%token <tok_val> EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%token <tok_val> LEFTDIV EMUL EDIV ELEFTDIV EPLUS EMINUS
%token <tok_val> QUOTE TRANSPOSE
%token <tok_val> PLUS_PLUS MINUS_MINUS POW EPOW
%token <tok_val> NUM IMAG_NUM
%token <tok_val> STRUCT_ELT
%token <tok_val> NAME
%token <tok_val> END
%token <tok_val> DQ_STRING SQ_STRING
%token <tok_val> FOR PARFOR WHILE DO UNTIL
%token <tok_val> IF ELSEIF ELSE
%token <tok_val> SWITCH CASE OTHERWISE
%token <tok_val> BREAK CONTINUE FUNC_RET
%token <tok_val> UNWIND CLEANUP
%token <tok_val> TRY CATCH
%token <tok_val> GLOBAL STATIC
%token <tok_val> FCN_HANDLE
%token <tok_val> PROPERTIES METHODS EVENTS ENUMERATION
%token <tok_val> METAQUERY
%token <tok_val> SUPERCLASSREF
%token <tok_val> GET SET

// Other tokens.
%token END_OF_INPUT LEXICAL_ERROR
%token FCN SCRIPT_FILE FUNCTION_FILE CLASSDEF
// %token VARARGIN VARARGOUT
%token CLOSE_BRACE


// Precedence and associativity.
%right '=' ADD_EQ SUB_EQ MUL_EQ DIV_EQ LEFTDIV_EQ POW_EQ EMUL_EQ EDIV_EQ ELEFTDIV_EQ EPOW_EQ OR_EQ AND_EQ LSHIFT_EQ RSHIFT_EQ
%left EXPR_OR_OR
%left EXPR_AND_AND
%left EXPR_OR
%left EXPR_AND
%left EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%left LSHIFT RSHIFT
%left ':'
%left '-' '+' EPLUS EMINUS
%left '*' '/' LEFTDIV EMUL EDIV ELEFTDIV
%right UNARY EXPR_NOT
%left POW EPOW QUOTE TRANSPOSE
%right PLUS_PLUS MINUS_MINUS
%left '(' '.' '{'

// Where to start.
%start input

%%

// ==============================
// Statements and statement lists
// ==============================

input           : input1
                  
                | function_file
                  
                | simple_list parse_error
                  
                | parse_error
                  
                ;

input1          : '\n'
                  
                | END_OF_INPUT
                  
                | simple_list
                  
                | simple_list '\n'
                  
                | simple_list END_OF_INPUT
                  
                ;

simple_list     : simple_list1 opt_sep_no_nl
                  
                ;

simple_list1    : statement
                  
                | simple_list1 sep_no_nl statement
                  
                ;

opt_list        : // empty
                  
                | list
                  
                ;

list            : list1 opt_sep
                  
                ;

list1           : statement
                  
                | list1 sep statement
                  
                ;

statement       : expression
                  
                | command
                  
                | word_list_cmd
                  
                ;

// =================
// Word-list command
// =================

// These are not really like expressions since they can't appear on
// the RHS of an assignment.  But they are also not like commands (IF,
// WHILE, etc.

word_list_cmd   : identifier word_list
                  
                ;

word_list       : string
                  
                | word_list string
                  
                ;

// ===========
// Expressions
// ===========

identifier      : NAME
                  
                ;

superclass_identifier
                : SUPERCLASSREF
                  
                ;

meta_identifier : METAQUERY
                  
                ;

string          : DQ_STRING
                  
                | SQ_STRING
                  
                ;

constant        : NUM
                  
                | IMAG_NUM
                  
                | string
                  
                ;

matrix          : '[' ']'
                  
                | '[' ';' ']'
                  
                | '[' ',' ']'
                  
                | '[' matrix_rows ']'
                  
                ;

matrix_rows     : matrix_rows1
                  
                | matrix_rows1 ';'      // Ignore trailing semicolon.
                  
                ;

matrix_rows1    : cell_or_matrix_row
                  
                | matrix_rows1 ';' cell_or_matrix_row
                  
                ;

cell            : '{' '}'
                  
                | '{' ';' '}'
                  
                | '{' cell_rows '}'
                  
                ;

cell_rows       : cell_rows1
                  
                | cell_rows1 ';'        // Ignore trailing semicolon.
                  
                ;

cell_rows1      : cell_or_matrix_row
                  
                | cell_rows1 ';' cell_or_matrix_row
                  
                ;

cell_or_matrix_row
                : arg_list
                  
                | arg_list ','  // Ignore trailing comma.
                  
                ;

fcn_handle      : '@' FCN_HANDLE
                  
                ;

anon_fcn_handle : '@' param_list statement
                  
                ;

primary_expr    : identifier
                  
                | constant
                  
                | fcn_handle
                  
                | matrix
                  
                | cell
                  
                | meta_identifier
                  
                | superclass_identifier
                  
                | '(' expression ')'
                  
                ;

magic_colon     : ':'
                  
                ;

magic_tilde     : EXPR_NOT
                  
                ;

arg_list        : expression
                  
                | magic_colon
                  
                | magic_tilde
                  
                | arg_list ',' magic_colon
                  
                | arg_list ',' magic_tilde
                  
                | arg_list ',' expression
                  
                ;

indirect_ref_op : '.'
                  
                ;

oper_expr       : primary_expr
                  
                | oper_expr PLUS_PLUS
                  
                | oper_expr MINUS_MINUS
                  
                | oper_expr '(' ')'
                  
                | oper_expr '(' arg_list ')'
                  
                | oper_expr '{' '}'
                | oper_expr '{' arg_list '}'
                | oper_expr QUOTE
                  
                | oper_expr TRANSPOSE
                  
                | oper_expr indirect_ref_op STRUCT_ELT
                  
                | oper_expr indirect_ref_op '(' expression ')'
                  
                | PLUS_PLUS oper_expr %prec UNARY
                  
                | MINUS_MINUS oper_expr %prec UNARY
                  
                | EXPR_NOT oper_expr %prec UNARY
                  
                | '+' oper_expr %prec UNARY
                  
                | '-' oper_expr %prec UNARY
                  
                | oper_expr POW oper_expr
                  
                | oper_expr EPOW oper_expr
                  
                | oper_expr '+' oper_expr
                  
                | oper_expr '-' oper_expr
                  
                | oper_expr '*' oper_expr
                  
                | oper_expr '/' oper_expr
                  
                | oper_expr EPLUS oper_expr
                  
                | oper_expr EMINUS oper_expr
                  
                | oper_expr EMUL oper_expr
                  
                | oper_expr EDIV oper_expr
                  
                | oper_expr LEFTDIV oper_expr
                  
                | oper_expr ELEFTDIV oper_expr
                  
                ;

colon_expr      : colon_expr1
                  
                ;

colon_expr1     : oper_expr
                  
                | colon_expr1 ':' oper_expr
                  
                ;

simple_expr     : colon_expr
                  
                | simple_expr LSHIFT simple_expr
                  
                | simple_expr RSHIFT simple_expr
                  
                | simple_expr EXPR_LT simple_expr
                  
                | simple_expr EXPR_LE simple_expr
                  
                | simple_expr EXPR_EQ simple_expr
                  
                | simple_expr EXPR_GE simple_expr
                  
                | simple_expr EXPR_GT simple_expr
                  
                | simple_expr EXPR_NE simple_expr
                  
                | simple_expr EXPR_AND simple_expr
                  
                | simple_expr EXPR_OR simple_expr
                  
                | simple_expr EXPR_AND_AND simple_expr
                  
                | simple_expr EXPR_OR_OR simple_expr
                  
                ;

// Arrange for the lexer to return CLOSE_BRACE for `]' by looking ahead
// one token for an assignment op.

assign_lhs      : simple_expr
                  
                | '[' arg_list CLOSE_BRACE
                  
                ;

assign_expr     : assign_lhs '=' expression
                  
                | assign_lhs ADD_EQ expression
                  
                | assign_lhs SUB_EQ expression
                  
                | assign_lhs MUL_EQ expression
                  
                | assign_lhs DIV_EQ expression
                  
                | assign_lhs LEFTDIV_EQ expression
                  
                | assign_lhs POW_EQ expression
                  
                | assign_lhs LSHIFT_EQ expression
                  
                | assign_lhs RSHIFT_EQ expression
                  
                | assign_lhs EMUL_EQ expression
                  
                | assign_lhs EDIV_EQ expression
                  
                | assign_lhs ELEFTDIV_EQ expression
                  
                | assign_lhs EPOW_EQ expression
                  
                | assign_lhs AND_EQ expression
                  
                | assign_lhs OR_EQ expression
                  
                ;

expression      : simple_expr
                  
                | assign_expr
                  
                | anon_fcn_handle
                  
                ;

// ================================================
// Commands, declarations, and function definitions
// ================================================

command         : declaration
                  
                | select_command
                  
                | loop_command
                  
                | jump_command
                  
                | except_command
                  
                | function
                  
                | script_file
                  
                | classdef
                  
                ;

// =====================
// Declaration statemnts
// =====================

parsing_decl_list
                : // empty
                  

declaration     : GLOBAL parsing_decl_list decl1
                  
                | STATIC parsing_decl_list decl1
                  
                ;

decl1           : decl2
                  
                | decl1 decl2
                  
                ;

decl_param_init : // empty
                

decl2           : identifier
                  
                | identifier '=' decl_param_init expression
                  
                | magic_tilde
                  
                ;

// ====================
// Selection statements
// ====================

select_command  : if_command
                  
                | switch_command
                  
                ;

// ============
// If statement
// ============

if_command      : IF stash_comment if_cmd_list END
                  
                ;

if_cmd_list     : if_cmd_list1
                  
                | if_cmd_list1 else_clause
                  
                ;

if_cmd_list1    : expression opt_sep opt_list
                  
                | if_cmd_list1 elseif_clause
                  
                ;

elseif_clause   : ELSEIF stash_comment opt_sep expression opt_sep opt_list
                  
                ;

else_clause     : ELSE stash_comment opt_sep opt_list
                  
                ;

// ================
// Switch statement
// ================

switch_command  : SWITCH stash_comment expression opt_sep case_list END
                  
                ;

case_list       : // empty
                  
                | default_case
                  
                | case_list1
                  
                | case_list1 default_case
                  
                ;

case_list1      : switch_case
                  
                | case_list1 switch_case
                  
                ;

switch_case     : CASE stash_comment opt_sep expression opt_sep opt_list
                  
                ;

default_case    : OTHERWISE stash_comment opt_sep opt_list
                  
                ;

// =======
// Looping
// =======

loop_command    : WHILE stash_comment expression opt_sep opt_list END
                  
                | DO stash_comment opt_sep opt_list UNTIL expression
                  
                | FOR stash_comment assign_lhs '=' expression opt_sep opt_list END
                  
                | FOR stash_comment '(' assign_lhs '=' expression ')' opt_sep opt_list END
                  
                | PARFOR stash_comment assign_lhs '=' expression opt_sep opt_list END
                  
                | PARFOR stash_comment '(' assign_lhs '=' expression ',' expression ')' opt_sep opt_list END
                  
                ;

// =======
// Jumping
// =======

jump_command    : BREAK
                  
                | CONTINUE
                  
                | FUNC_RET
                  
                ;

// ==========
// Exceptions
// ==========

except_command  : UNWIND stash_comment opt_sep opt_list CLEANUP
                  stash_comment opt_sep opt_list END
                  
                | TRY stash_comment opt_sep opt_list CATCH
                  stash_comment opt_sep opt_list END
                  
                | TRY stash_comment opt_sep opt_list END
                  
                ;

// ===========================================
// Some `subroutines' for function definitions
// ===========================================

push_fcn_symtab : // empty
                  
                ;

// ===========================
// List of function parameters
// ===========================

param_list_beg  : '('
                  
                ;

param_list_end  : ')'
                  
                ;

param_list      : param_list_beg param_list1 param_list_end
                  
                | param_list_beg error
                  
                ;

param_list1     : // empty
                  
                | param_list2
                  
                ;

param_list2     : decl2
                  
                | param_list2 ',' decl2
                  
                ;

// ===================================
// List of function return value names
// ===================================

return_list     : '[' ']'
                  
                | return_list1
                  
                | '[' return_list1 ']'
                  
                ;

return_list1    : identifier
                  
                | return_list1 ',' identifier
                  
                ;

// ===========
// Script file
// ===========

script_file     : SCRIPT_FILE opt_list END_OF_INPUT
                  
                ;

// =============
// Function file
// =============

function_file   : FUNCTION_FILE function_list opt_sep END_OF_INPUT
                  
                ;

function_list   : function
                | function_list sep function
                ;

// ===================
// Function definition
// ===================

function_beg    : push_fcn_symtab FCN stash_comment
                  
                ;

function        : function_beg function1
                  
                | function_beg return_list '=' function1
                  
                ;

fcn_name        : identifier
                  
                | GET '.' identifier
                  
                | SET '.' identifier
                  
                ;

function1       : fcn_name function2
                  
                ;

function2       : param_list opt_sep opt_list function_end
                  
                | opt_sep opt_list function_end
                  
                ;

function_end    : END
                  
                | END_OF_INPUT
                  
                ;

// ========
// Classdef
// ========

classdef_beg    : CLASSDEF stash_comment
                  
                ;

classdef_end    : END
                  
                ;

classdef1       : classdef_beg opt_attr_list identifier opt_superclasses
                  
                ;

classdef        : classdef1 opt_sep class_body opt_sep stash_comment classdef_end
                  
                ;

opt_attr_list   : // empty
                  
                | '(' attr_list ')'
                  
                ;

attr_list       : attr
                  
                | attr_list ',' attr
                  
                ;

attr            : identifier
                  
                | identifier '=' decl_param_init expression
                  
                | EXPR_NOT identifier
                  
                ;

opt_superclasses
                : // empty
                  
                | superclasses
                  
                ;

superclasses    : EXPR_LT identifier '.' identifier
                  
                | EXPR_LT identifier
                  
                | superclasses EXPR_AND identifier '.' identifier
                  
                | superclasses EXPR_AND identifier
                  
                ;

class_body      : properties_block
                  
                | methods_block
                  
                | events_block
                  
                | enum_block
                  
                | class_body opt_sep properties_block
                  
                | class_body opt_sep methods_block
                  
                | class_body opt_sep events_block
                  
                | class_body opt_sep enum_block
                  
                ;

properties_beg  : PROPERTIES stash_comment
                  
                ;

properties_block
                : properties_beg opt_attr_list opt_sep properties_list opt_sep END
                  
                ;

properties_list
                : class_property
                  
                | properties_list opt_sep class_property
                  
                ;

class_property  : identifier
                  
                | identifier '=' decl_param_init expression ';'
                  
                ;

methods_beg     : METHODS stash_comment
                  
                ;

methods_block   : methods_beg opt_attr_list opt_sep methods_list opt_sep END
                  
                ;

methods_list    : function
                  
                | methods_list opt_sep function
                  
                ;

events_beg      : EVENTS stash_comment
                  
                ;

events_block    : events_beg opt_attr_list opt_sep events_list opt_sep END
                  
                ;

events_list     : class_event
                  
                | events_list opt_sep class_event
                  
                ;

class_event     : identifier
                  
                ;

enum_beg        : ENUMERATION stash_comment
                  
                ;

enum_block      : enum_beg opt_attr_list opt_sep enum_list opt_sep END
                  
                ;

enum_list       : class_enum
                  
                | enum_list opt_sep class_enum
                  
                ;

class_enum      : identifier '(' expression ')'
                  
                ;

// =============
// Miscellaneous
// =============

stash_comment   : // empty
                  
                ;

parse_error     : LEXICAL_ERROR
                  
                | error
                ;

sep_no_nl       : ','
                  
                | ';'
                  
                | sep_no_nl ','
                  
                | sep_no_nl ';'
                  
                ;

opt_sep_no_nl   : // empty
                  
                | sep_no_nl
                  
                ;

sep             : ','
                  
                | ';'
                  
                | '\n'
                  
                | sep ','
                  
                | sep ';'
                  
                | sep '\n'
                  
                ;

opt_sep         : // empty
                  
                | sep
                  
                ;

%%
