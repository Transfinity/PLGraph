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
#include "gst.h"
#include "gstpriv.h"
#include <stdio.h>
#if defined(STDC_HEADERS)
#include <string.h>
#endif

#define YYPRINT(file, type, value)   _gst_yyprint (file, type, &value)

#define YYERROR_VERBOSE 1

static inline mst_Boolean           is_unlikely_selector (register const char *str);
%}

%name-prefix="_gst_yy"
%debug
%pure_parser

/* definition of YYSTYPE */
%union{
  char		cval;
  long double	fval;
  long		ival;
  char		*sval;
  byte_object	boval;
  OOP		oval;
  tree_node	node;
}

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

%type <node> method message_pattern variable_name keyword_variable_list
	temporaries variable_names statements 
	statements.1 statement expression return_statement
	assigns primary number small_number symbol_constant symbol
	character_constant string array_constant array
	array_constant_list byte_array byte_array_constant_list
	block opt_block_variables array_constructor
	block_variable_list unary_expression binary_expression
	keyword_expression keyword_binary_object_description_list
	cascaded_message_expression semi_message_list
	message_elt simple_expression literal message_expression
	array_constant_elt unary_object_description
	binary_object_description variable_binding 
	variable_primary compile_time_constant
	compile_time_constant_body

%type <sval> unary_selector keyword binary_selector primitive
%%

program:
	internal_marker method
	| file_in
	| /* empty */
	;

internal_marker:
	INTERNAL_TOKEN
		{
		  _gst_clear_method_start_pos(); 
		}
	;

file_in:
	doit_and_method_list
	| file_in doit_and_method_list
	;

doit_and_method_list:
	doit
	| doit internal_marker method_list '!'
		{
		  _gst_skip_compilation = false;
		  _gst_set_compilation_class(_gst_undefined_object_class);
		}

	;

doit:
	temporaries statements '!'

		{
		  if ($2 && !_gst_had_error)
		    _gst_execute_statements($1, $2, false);

		  _gst_free_tree();
		  _gst_had_error = false;
		}
	| error '!'
		{
		  _gst_had_error = false;
		  yyerrok;
		}
	;

method_list:
        method '!' method_list
		{
		}
	| error '!'
		{
		  _gst_had_error = false;
		  yyerrok;
		}
	| /* EMPTY */
	;
     
method:
	message_pattern temporaries primitive statements
		{
		  $$ = _gst_make_method(&@$, $1, $2, $3, $4); 
		  if (!_gst_had_error && !_gst_skip_compilation) {
		    _gst_compile_method($$, false, true);
		    _gst_clear_method_start_pos();
		  }

		  _gst_free_tree();
		  _gst_had_error = false;
		}
	;

message_pattern:
	unary_selector
		{
		  $$ = _gst_make_unary_expr(&@$, NULL, $1); 
		}
	| binary_selector variable_name
		{
		  $$ = _gst_make_binary_expr(&@$, NULL, $1, $2); 
		}
	| keyword_variable_list
		{
		  $$ = _gst_make_keyword_expr(&@$, NULL, $1); 
		}
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
		{
		  $$ = _gst_make_variable(&@$, $1); 
		}
	;

keyword_variable_list:
	keyword variable_name
		{
		  $$ = _gst_make_keyword_list(&@$, $1, $2); 
		}
	| keyword_variable_list keyword variable_name
		{
		  _gst_add_node($1, _gst_make_keyword_list(&@$, $2, $3));
		  $$ = $1; 
		}
	;

keyword:
	KEYWORD
	;

primitive:
	/* empty */
		{
		  $$ = 0; 
		}
	| PRIMITIVE_START IDENTIFIER primitive_end
		{
		  $$ = $2; 
		}
	;

primitive_end: BINOP
		{
		  if (strcmp($1, ">") != 0) YYFAIL; 
		}
	;

temporaries:
	/* empty */
		{
		  $$ = NULL; 
		}
	| '|' '|'
		{
		  $$ = NULL; 
		}
	| '|' variable_names '|'
		{
		  $$ = $2; 
		}
	;

variable_names:
	variable_name
		{
		  $$ = _gst_make_variable_list(&@$, $1); 
		}
	| variable_names variable_name
		{
		  _gst_add_node($1, _gst_make_variable_list(&@$, $2));
		  $$ = $1; 
		}
	;

statements:
	/* empty */
		{
		  $$ = NULL; 
		}
	| return_statement optional_dot	
		{
		  $$ = $1;
		}
	| statements.1 optional_dot
		{
		  $$ = $1;
		}
	| statements.1 '.' return_statement optional_dot	
		{
		  _gst_add_node ($1, $3);
		  $$ = $1;
		}
	;

statements.1:
	statement
		{
		  $$ = $1; 
		}
	| statements.1 '.' statement
		{
		  _gst_add_node($1, $3); 
		  $$ = $1;
		}
	| statements.1 '.' error
		{
		  $$ = $1;
		  yyerrok;
		  _gst_had_error = true;
		}
	;
	
optional_dot:
	/* empty */
 	| '.'
	;

statement:
	expression
		{
		  $$ = _gst_make_statement_list(&@$, $1); 
		}
	;

return_statement:
	'^' expression 
		{
		  $$ = _gst_make_statement_list(&@$,
						_gst_make_return(&@$, $2));
		}
	;

expression:
	simple_expression
	| assigns simple_expression
		{
		  $$ = _gst_make_assign(&@$, $1, $2); 
		}
	;

assigns:
	variable_primary ASSIGNMENT
		{
		  $$ = _gst_make_assignment_list(&@$, $1); 
		}
	| assigns variable_primary ASSIGNMENT
		{
		  _gst_add_node($1, _gst_make_assignment_list(&@$, $2));
		  $$ = $1; 
		}
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
		{
		  yyclearin;
		  _gst_unread_char ('!');
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '(' error ')'
		{
		  yyerrok;
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '(' expression ')'
		{
		  $$ = $2; 
		}
	;

variable_primary:
	IDENTIFIER
		{
		  $$ = _gst_make_variable(&@$, $1); 
		}
	| variable_primary SCOPE_SEPARATOR IDENTIFIER
		{
		  _gst_add_node($1, _gst_make_variable(&@$, $3)); 
		}
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
		{
		  $$ = _gst_make_int_constant(&@$, $1); 
		}
	| FLOATD_LITERAL
		{
		  $$ = _gst_make_float_constant(&@$, $1, CONST_FLOATD); 
		}
	| FLOATE_LITERAL
		{
		  $$ = _gst_make_float_constant(&@$, $1, CONST_FLOATE); 
		}
	| FLOATQ_LITERAL
		{
		  $$ = _gst_make_float_constant(&@$, $1, CONST_FLOATQ); 
		}
	| LARGE_INTEGER_LITERAL
		{
		  $$ = _gst_make_byte_object_constant(&@$, $1); 
		}
	| SCALED_DECIMAL_LITERAL
		{
		  $$ = _gst_make_oop_constant(&@$, $1); 
		}
	;

small_number:
	BYTE_LITERAL
		{
		  $$ = _gst_make_int_constant(&@$, $1); 
		}
	;

symbol_constant:
	'#' symbol
		{
		  $$ = _gst_make_symbol_constant(&@$, $2); 
		}
	| '#' STRING_LITERAL
		{
		  $$ = _gst_make_symbol_constant(&@$, _gst_intern_ident(&@$, $2)); 
		}
	;

symbol:
	IDENTIFIER
		{
		  $$ = _gst_intern_ident(&@$, $1); 
		}
	| binary_selector
		{
		  $$ = _gst_intern_ident(&@$, $1); 
		}
	| SYMBOL_KEYWORD
		{
		  $$ = _gst_intern_ident(&@$, $1); 
		}
	| KEYWORD
		{
		  $$ = _gst_intern_ident(&@$, $1); 
		}
	;


character_constant:
	CHAR_LITERAL
		{
		  $$ = _gst_make_char_constant(&@$, $1); 
		}
	;

string:
	STRING_LITERAL
		{
		  $$ = _gst_make_string_constant(&@$, $1); 
		}
	;

array_constant:
	'#' array
		{
		  $$ = $2; 
		}
	| '#' byte_array
		{
		  $$ = $2; 
		}
	;

array:
	'(' ')'
		{
		  $$ = _gst_make_array_constant(&@$, NULL); 
		}
	| '(' error '!'
		{
		  yyclearin;
		  _gst_unread_char ('!');
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '(' error ')'
		{
		  yyerrok;
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '(' array_constant_list ')'
		{
		  $$ = _gst_make_array_constant(&@$, $2); 
		}
	;

array_constant_list:
	array_constant_elt
		{
		  $$ = _gst_make_array_elt(&@$, $1); 
		}
	| array_constant_list array_constant_elt
		{
		  _gst_add_node($1, _gst_make_array_elt(&@$, $2));
		  $$ = $1; 
		}
	;

array_constant_elt:
	array
	| byte_array
	| literal
	| symbol
		{
		  OOP symbolOOP = $1->v_expr.selector;
		  if (symbolOOP == _gst_true_symbol) {
		    $$ = _gst_make_oop_constant(&@$, _gst_true_oop);
		  } else if (symbolOOP == _gst_false_symbol) {
		    $$ = _gst_make_oop_constant(&@$, _gst_false_oop);
		  } else if (symbolOOP == _gst_nil_symbol) {
		    $$ = _gst_make_oop_constant(&@$, _gst_nil_oop);
		  } else {
		    _gst_errorf ("expected true, false or nil");
		    YYERROR;
		  }
		}
	;

byte_array:
	'[' ']'
		{
		  $$ = _gst_make_byte_array_constant(&@$, NULL); 
		}
	| '[' error '!'
		{
		  yyclearin;
		  _gst_unread_char ('!');
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '[' error ']'
		{
		  yyerrok;
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '[' byte_array_constant_list ']'
		{
		  $$ = _gst_make_byte_array_constant(&@$, $2); 
		}
	;

byte_array_constant_list:
	small_number
		{
		  $$ = _gst_make_array_elt(&@$, $1); 
		}
	| byte_array_constant_list small_number
		{
		  _gst_add_node($1, _gst_make_array_elt(&@$, $2));
		  $$ = $1; 
		}
	;

variable_binding:
	'#' '{' error '!'
		{
		  yyclearin;
		  _gst_unread_char ('!');
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '#' '{' error '}'
		{
		  yyerrok;
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '#' '{' variable_primary '}'

		{
		  $$ = _gst_make_binding_constant(&@$, $3); 
		  if (!$$) {
		    _gst_errorf ("invalid variable binding");
		    YYERROR;
		  }
		}
	;

compile_time_constant:
	'#' '#' compile_time_constant_body
		{
		  $$ = $3;
		}
	;

compile_time_constant_body:
	'(' error '!'
		{
		  yyclearin;
		  _gst_unread_char ('!');
		  _gst_had_error = true;
		  $$ = _gst_make_oop_constant(&@$, _gst_nil_oop); 
		}
	| '(' error ')'
		{
		  yyerrok;
		  _gst_had_error = true;
		  $$ = _gst_make_oop_constant(&@$, _gst_nil_oop); 
		}
	| '(' temporaries statements ')'
		{
		  OOP result = _gst_nil_oop;
		  if ($3 && !_gst_had_error)
		    result = _gst_execute_statements($2, $3, true);

		  $$ = _gst_make_oop_constant(&@$, result); 
		}
	;

array_constructor:
	'{' error '!'
		{
		  yyclearin;
		  _gst_unread_char ('!');
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '{' error '}'
		{
		  yyerrok;
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '{' statements '}'
		{
		  $$ = _gst_make_array_constructor(&@$, $2); 
		}
	;

block:
	'[' error '!'
		{
		  yyclearin;
		  _gst_unread_char ('!');
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '[' error ']'
		{
		  yyerrok;
		  _gst_had_error = true;
		  $$ = NULL; 
		}
	| '[' opt_block_variables temporaries statements ']'
		{
		  $$ = _gst_make_block(&@$, $2, $3, $4); 
		}
	;

opt_block_variables:
	/* empty */
		{
		  $$ = NULL; 
		}
	| block_variable_list binary_selector
		{
		  if ($2[0] != '|') {
		    YYFAIL;
		  } else if ($2[1] == '\0') {	/* | */
		  } else if ($2[1] == '|') {   /* || */
		    _gst_unread_char('|');
		  }
		}
	;

/* syntax for blocks with temporaries is just args and vertical bar (if
 * any followed by a standard temporaries declarations */

block_variable_list:
	':' variable_name
		{
		  $$ = _gst_make_variable_list(&@$, $2); 
		}
	| block_variable_list ':' variable_name
		{
		  _gst_add_node($1, _gst_make_variable_list(&@$, $3));
		  $$ = $1; 
		}
	;

message_expression:
	unary_expression
	| binary_expression
	| keyword_expression
	;

unary_expression:
	unary_object_description unary_selector
		{
		  if (is_unlikely_selector ($2)) {
		    _gst_warningf ("sending `%s', most likely you "
			           "forgot a period", $2);
		  }
		  $$ = _gst_make_unary_expr(&@$, $1, $2); 
		}
	;

unary_object_description:
	primary
	| unary_expression
	;

binary_expression:
	binary_object_description binary_selector unary_object_description
		{
		  $$ = _gst_make_binary_expr(&@$, $1, $2, $3); 
		}
	;

binary_object_description:
	unary_object_description
	| binary_expression
	;

keyword_expression:
	binary_object_description keyword_binary_object_description_list
		{
		  $$ = _gst_make_keyword_expr(&@$, $1, $2); 
		}
 	;

keyword_binary_object_description_list:
	keyword binary_object_description
		{
		  $$ = _gst_make_keyword_list(&@$, $1, $2); 
		}
	| keyword_binary_object_description_list keyword
	  binary_object_description
		{
		   _gst_add_node($1, _gst_make_keyword_list(&@$, $2, $3));
		   $$ = $1; 
		}
	;

cascaded_message_expression:
	message_expression semi_message_list
		{
		  $$ = _gst_make_cascaded_message(&@$, $1, $2); 
		}
	;

semi_message_list:
	';' message_elt
		{
		  $$ = _gst_make_message_list(&@$, $2); 
		}
	| semi_message_list ';' message_elt
		{
		  _gst_add_node($1, _gst_make_message_list(&@$, $3));
		  $$ = $1; 
		}
	;

message_elt:
	unary_selector
		{
		   $$ = _gst_make_unary_expr(&@$, NULL, $1); 
		}
	| binary_selector unary_object_description
		{
		  $$ = _gst_make_binary_expr(&@$, NULL, $1, $2); 
		}
	| keyword_binary_object_description_list
		{
		  $$ = _gst_make_keyword_expr(&@$, NULL, $1); 
		}
	;


%%
/*     
 * ADDITIONAL C CODE
 */

/* Based on an hash table produced by gperf version 2.7.2
   Command-line: gperf -tn -F ', false' -j1 -k1,2
   with the following input:

   false
   nil
   self
   super
   thisContext
   true
   fe
   ne
   nh
   sr

   A few negatives have been included in the input to avoid that
   messages like #new or #size require a strcmp (their hash value is
   in range if only the six keywords were included), and the length
   has not been included to make the result depend on selectors
   *starting* with two given letters.  With this hash table and this
   implementation, only selectors starting with "fa", "ni", "se",
   "su", "th", "tr" (which are unavoidable) require a strcmp, which is
   a good compromise.  All the others require three array lookups
   (two for the hash function, one to check for the first character)

   An alternative could have been simple trie-like
   code like this:

   return ((*$1 == 's' &&
	    (strcmp($1+1, "elf") == 0 ||
	     strcmp($1+1, "uper") == 0)) ||
   
	   (*$1 == 't' &&
	    (strcmp($1+1, "rue") == 0 ||
	     strcmp($1+1, "hisContext") == 0)) ||
   
	   (*$1 == 'f' && strcmp($1+1, "alse") == 0) ||
	   (*$1 == 'n' && strcmp($1+1, "il") == 0))

   ... but using gperf is more cool :-) */

mst_Boolean
is_unlikely_selector (register const char *str)
{
  /* The first-character table is big enough that
     we skip the range check on the hash value */

  static char first[31] = 
    "s  s   f  n  tt               ";

  static char *rest[] =
    {
      "elf",
      NULL,
      NULL,
      "uper",
      NULL,
      NULL,
      NULL,
      "alse",
      NULL,
      NULL,
      "il",
      NULL,
      NULL,
      "hisContext",
      "rue"
    };

  static unsigned char asso_values[] =
    {
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15,  1, 15, 15,
      15,  0,  6, 15,  4,  2, 15, 15, 15, 15,
       8, 15, 15, 15,  5,  0,  9,  3, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15
    };

  register int key = asso_values[(unsigned char)str[1]] + 
    asso_values[(unsigned char)str[0]];

  return
    first[key] == *str &&
    !strcmp (str + 1, rest[key]);
}
