/* Source code parsing and tree node generation for the GNU compiler
   for the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* This file parses java source code and issues a tree node image
suitable for code generation (byte code and targeted CPU assembly
language).

The grammar conforms to the Java grammar described in "The Java(TM)
Language Specification. J. Gosling, B. Joy, G. Steele. Addison Wesley
1996, ISBN 0-201-63451-1"

The following modifications were brought to the original grammar:

method_body: added the rule '| block SC_TK'
static_initializer: added the rule 'static block SC_TK'.

Note: All the extra rules described above should go away when the
      empty_statement rule will work.

statement_nsi: 'nsi' should be read no_short_if.

Some rules have been modified to support JDK1.1 inner classes
definitions and other extensions.  */

%{
%}


%pure_parser

/* Things defined here have to match the order of what's in the
   binop_lookup table.  */

%token   PLUS_TK         MINUS_TK        MULT_TK         DIV_TK    REM_TK
%token   LS_TK           SRS_TK          ZRS_TK
%token   AND_TK          XOR_TK          OR_TK
%token   BOOL_AND_TK BOOL_OR_TK
%token   EQ_TK NEQ_TK GT_TK GTE_TK LT_TK LTE_TK

/* This maps to the same binop_lookup entry than the token above */

%token   PLUS_ASSIGN_TK  MINUS_ASSIGN_TK MULT_ASSIGN_TK DIV_ASSIGN_TK
%token   REM_ASSIGN_TK
%token   LS_ASSIGN_TK    SRS_ASSIGN_TK   ZRS_ASSIGN_TK
%token   AND_ASSIGN_TK   XOR_ASSIGN_TK   OR_ASSIGN_TK


/* Modifier TOKEN have to be kept in this order. Don't scramble it */

%token   PUBLIC_TK       PRIVATE_TK         PROTECTED_TK
%token   STATIC_TK       FINAL_TK           SYNCHRONIZED_TK
%token   VOLATILE_TK     TRANSIENT_TK       NATIVE_TK
%token   PAD_TK          ABSTRACT_TK        STRICT_TK
%token   MODIFIER_TK

/* Keep those two in order, too */
%token   DECR_TK INCR_TK

/* From now one, things can be in any order */

%token   DEFAULT_TK      IF_TK              THROW_TK
%token   BOOLEAN_TK      DO_TK              IMPLEMENTS_TK
%token   THROWS_TK       BREAK_TK           IMPORT_TK
%token   ELSE_TK         INSTANCEOF_TK      RETURN_TK
%token   VOID_TK         CATCH_TK           INTERFACE_TK
%token   CASE_TK         EXTENDS_TK         FINALLY_TK
%token   SUPER_TK        WHILE_TK           CLASS_TK
%token   SWITCH_TK       CONST_TK           TRY_TK
%token   FOR_TK          NEW_TK             CONTINUE_TK
%token   GOTO_TK         PACKAGE_TK         THIS_TK
%token   ASSERT_TK

%token   BYTE_TK         SHORT_TK           INT_TK            LONG_TK
%token   CHAR_TK         INTEGRAL_TK

%token   FLOAT_TK        DOUBLE_TK          FP_TK

%token   ID_TK

%token   REL_QM_TK         REL_CL_TK NOT_TK  NEG_TK

%token   ASSIGN_ANY_TK   ASSIGN_TK
%token   OP_TK  CP_TK  OCB_TK  CCB_TK  OSB_TK  CSB_TK  SC_TK  C_TK DOT_TK

%token   STRING_LIT_TK   CHAR_LIT_TK        INT_LIT_TK        FP_LIT_TK
%token   TRUE_TK         FALSE_TK           BOOL_LIT_TK       NULL_TK


%%
/* 19.2 Production from 2.3: The Syntactic Grammar  */
goal:
                
	compilation_unit
		
;

/* 19.3 Productions from 3: Lexical structure  */
literal:
	INT_LIT_TK
|	FP_LIT_TK
|	BOOL_LIT_TK
|	CHAR_LIT_TK
|       STRING_LIT_TK
|       NULL_TK
;

/* 19.4 Productions from 4: Types, Values and Variables  */
type:
	primitive_type
|	reference_type
;

primitive_type:
	INTEGRAL_TK
|	FP_TK
|	BOOLEAN_TK
;

reference_type:
	class_or_interface_type
|	array_type
;

class_or_interface_type:
	name
;

class_type:
	class_or_interface_type	/* Default rule */
;

interface_type:
	 class_or_interface_type
;

array_type:
	primitive_type dims
		
|	name dims
		
;

/* 19.5 Productions from 6: Names  */
name:
	simple_name		/* Default rule */
|	qualified_name		/* Default rule */
;

simple_name:
	identifier		/* Default rule */
;

qualified_name:
	name DOT_TK identifier
		
;

identifier:
	ID_TK
;

/* 19.6: Production from 7: Packages  */
compilation_unit:
		
|	package_declaration
|	import_declarations
|	type_declarations
|       package_declaration import_declarations
|       package_declaration type_declarations
|       import_declarations type_declarations
|       package_declaration import_declarations type_declarations
;

import_declarations:
	import_declaration
		
|	import_declarations import_declaration
		
;

type_declarations:
	type_declaration
| 	type_declarations type_declaration
;

package_declaration:
	PACKAGE_TK name SC_TK
		
|	PACKAGE_TK error
		
|	PACKAGE_TK name error
		
;

import_declaration:
	single_type_import_declaration
|	type_import_on_demand_declaration
;

single_type_import_declaration:
	IMPORT_TK name SC_TK
|	IMPORT_TK error
		
|	IMPORT_TK name error
		
;

type_import_on_demand_declaration:
	IMPORT_TK name DOT_TK MULT_TK SC_TK
|	IMPORT_TK name DOT_TK error
|	IMPORT_TK name DOT_TK MULT_TK error
;

type_declaration:
	class_declaration
|	interface_declaration
|	empty_statement
|	error
;

/* 19.7 Shortened from the original:
   modifiers: modifier | modifiers modifier
   modifier: any of public...  */
modifiers:
	MODIFIER_TK
|	modifiers MODIFIER_TK
;

/* 19.8.1 Production from $8.1: Class Declaration */
class_declaration:
	modifiers CLASS_TK identifier super interfaces
	class_body
|	CLASS_TK identifier super interfaces
	class_body
|	modifiers CLASS_TK error
|	CLASS_TK error
|       CLASS_TK identifier error
|       modifiers CLASS_TK identifier error
;

super:
|	EXTENDS_TK class_type
		
|	EXTENDS_TK class_type error
|	EXTENDS_TK error
;

interfaces:
		
|	IMPLEMENTS_TK interface_type_list
		
|	IMPLEMENTS_TK error
		
;

interface_type_list:
	interface_type
		
|	interface_type_list C_TK interface_type
		
|	interface_type_list C_TK error
		
;

class_body:
	OCB_TK CCB_TK
		
|	OCB_TK class_body_declarations CCB_TK
		
;

class_body_declarations:
	class_body_declaration
|	class_body_declarations class_body_declaration
;

class_body_declaration:
	class_member_declaration
|	static_initializer
|	constructor_declaration
|	block			/* Added, JDK1.1, instance initializer */
		
;

class_member_declaration:
	field_declaration
|	method_declaration
|	class_declaration	/* Added, JDK1.1 inner classes */
		
|	interface_declaration	/* Added, JDK1.1 inner interfaces */
		
|	empty_statement
;

/* 19.8.2 Productions from 8.3: Field Declarations  */
field_declaration:
	type variable_declarators SC_TK
		
|	modifiers type variable_declarators SC_TK
		
;

variable_declarators:
	/* Should we use build_decl_list () instead ? FIXME */
	variable_declarator	/* Default rule */
|	variable_declarators C_TK variable_declarator
		
|	variable_declarators C_TK error
		
;

variable_declarator:
	variable_declarator_id
		
|	variable_declarator_id ASSIGN_TK variable_initializer
		
|	variable_declarator_id ASSIGN_TK error
		
|	variable_declarator_id ASSIGN_TK variable_initializer error
		
;

variable_declarator_id:
	identifier
|	variable_declarator_id OSB_TK CSB_TK
		
|	identifier error
		
|	variable_declarator_id OSB_TK error
		
|	variable_declarator_id CSB_TK error
		
;

variable_initializer:
	expression
|	array_initializer
;

/* 19.8.3 Productions from 8.4: Method Declarations  */
method_declaration:
	method_header
		
	method_body
		
|	method_header error
		
;

method_header:
	type method_declarator throws
		
|	VOID_TK method_declarator throws
		
|	modifiers type method_declarator throws
		
|	modifiers VOID_TK method_declarator throws
		
|	type error
		
|	modifiers type error
		
|	VOID_TK error
		
|	modifiers VOID_TK error
		
|	modifiers error
		
;

method_declarator:
	identifier OP_TK CP_TK
		
|	identifier OP_TK formal_parameter_list CP_TK
		
|	method_declarator OSB_TK CSB_TK
		
|	identifier OP_TK error
		
|	method_declarator OSB_TK error
		
;

formal_parameter_list:
	formal_parameter
		
|	formal_parameter_list C_TK formal_parameter
		
|	formal_parameter_list C_TK error
		
;

formal_parameter:
	type variable_declarator_id
		
|	final type variable_declarator_id /* Added, JDK1.1 final parms */
		
|	type error
		
|	final type error
		
;

final:
	modifiers
		
;

throws:
		
|	THROWS_TK class_type_list
		
|	THROWS_TK error
		
;

class_type_list:
	class_type
		
|	class_type_list C_TK class_type
		
|	class_type_list C_TK error
		
;

method_body:
	block
|	SC_TK 
;

/* 19.8.4 Productions from 8.5: Static Initializers  */
static_initializer:
	static block
		
;

static:				/* Test lval.sub_token here */
	modifiers
		
;

/* 19.8.5 Productions from 8.6: Constructor Declarations  */
constructor_declaration:
	constructor_header
		
	constructor_body
		
;

constructor_header:
	constructor_declarator throws
		
|	modifiers constructor_declarator throws
		
;

constructor_declarator:
	simple_name OP_TK CP_TK
		
|	simple_name OP_TK formal_parameter_list CP_TK
		
;

constructor_body:
	/* Unlike regular method, we always need a complete (empty)
	   body so we can safely perform all the required code
	   addition (super invocation and field initialization) */
	block_begin constructor_block_end
		
|	block_begin explicit_constructor_invocation constructor_block_end
		
|	block_begin block_statements constructor_block_end
		
|       block_begin explicit_constructor_invocation block_statements constructor_block_end
		
;

constructor_block_end:
	block_end
;

/* Error recovery for that rule moved down expression_statement: rule.  */
explicit_constructor_invocation:
	this_or_super OP_TK CP_TK SC_TK
		
|	this_or_super OP_TK argument_list CP_TK SC_TK
		
        /* Added, JDK1.1 inner classes. Modified because the rule
	   'primary' couldn't work.  */
|	name DOT_TK SUPER_TK OP_TK argument_list CP_TK SC_TK
		
|	name DOT_TK SUPER_TK OP_TK CP_TK SC_TK
		
;

this_or_super:			/* Added, simplifies error diagnostics */
	THIS_TK
		
|	SUPER_TK
		
;

/* 19.9 Productions from 9: Interfaces  */
/* 19.9.1 Productions from 9.1: Interfaces Declarations  */
interface_declaration:
	INTERFACE_TK identifier
		
	interface_body
		
|	modifiers INTERFACE_TK identifier
		
	interface_body
		
|	INTERFACE_TK identifier extends_interfaces
		
	interface_body
		
|	modifiers INTERFACE_TK identifier extends_interfaces
		
	interface_body
		
|	INTERFACE_TK identifier error
		
|	modifiers INTERFACE_TK identifier error
		
;

extends_interfaces:
	EXTENDS_TK interface_type
		
|	extends_interfaces C_TK interface_type
		
|	EXTENDS_TK error
		
|	extends_interfaces C_TK error
		
;

interface_body:
	OCB_TK CCB_TK
		
|	OCB_TK interface_member_declarations CCB_TK
		
;

interface_member_declarations:
	interface_member_declaration
|	interface_member_declarations interface_member_declaration
;

interface_member_declaration:
	constant_declaration
|	abstract_method_declaration
|	class_declaration	/* Added, JDK1.1 inner classes */
		
|	interface_declaration	/* Added, JDK1.1 inner interfaces */
		
;

constant_declaration:
	field_declaration
;

abstract_method_declaration:
	method_header SC_TK
		
|	method_header error
		
;

/* 19.10 Productions from 10: Arrays  */
array_initializer:
	OCB_TK CCB_TK
		
|	OCB_TK C_TK CCB_TK
		
|	OCB_TK variable_initializers CCB_TK
		
|	OCB_TK variable_initializers C_TK CCB_TK
		
;

variable_initializers:
	variable_initializer
		
|	variable_initializers C_TK variable_initializer
		
|	variable_initializers C_TK error
		
;

/* 19.11 Production from 14: Blocks and Statements  */
block:
	OCB_TK CCB_TK
		
|	block_begin block_statements block_end
		
;

block_begin:
	OCB_TK
		
;

block_end:
	CCB_TK
		
;

block_statements:
	block_statement
|	block_statements block_statement
;

block_statement:
	local_variable_declaration_statement
|	statement
		
|	class_declaration	/* Added, JDK1.1 local classes */
		
;

local_variable_declaration_statement:
	local_variable_declaration SC_TK /* Can't catch missing ';' here */
;

local_variable_declaration:
	type variable_declarators
		
|	final type variable_declarators /* Added, JDK1.1 final locals */
		
;

statement:
	statement_without_trailing_substatement
|	labeled_statement
|	if_then_statement
|	if_then_else_statement
|	while_statement
|	for_statement
		
;

statement_nsi:
	statement_without_trailing_substatement
|	labeled_statement_nsi
|	if_then_else_statement_nsi
|	while_statement_nsi
|	for_statement_nsi
		
;

statement_without_trailing_substatement:
	block
|	empty_statement
|	expression_statement
|	switch_statement
|	do_statement
|	break_statement
|	continue_statement
|	return_statement
|	synchronized_statement
|	throw_statement
|	try_statement
|	assert_statement
;

empty_statement:
	SC_TK
		
;

label_decl:
	identifier REL_CL_TK
		
;

labeled_statement:
	label_decl statement
		
|	identifier error
		
;

labeled_statement_nsi:
	label_decl statement_nsi
		
;

/* We concentrate here a bunch of error handling rules that we couldn't write
   earlier, because expression_statement catches a missing ';'.  */
expression_statement:
	statement_expression SC_TK
		
|	error SC_TK
		
|	error OCB_TK
		
|	error CCB_TK
		
|       this_or_super OP_TK error
		
|       this_or_super OP_TK CP_TK error
		
|       this_or_super OP_TK argument_list error
		
|       this_or_super OP_TK argument_list CP_TK error
		
|	name DOT_TK SUPER_TK error
		
|	name DOT_TK SUPER_TK OP_TK error
		
|	name DOT_TK SUPER_TK OP_TK argument_list error
		
|	name DOT_TK SUPER_TK OP_TK argument_list CP_TK error
		
|	name DOT_TK SUPER_TK OP_TK CP_TK error
		
;

statement_expression:
	assignment
|	pre_increment_expression
|	pre_decrement_expression
|	post_increment_expression
|	post_decrement_expression
|	method_invocation
|	class_instance_creation_expression
;

if_then_statement:
	IF_TK OP_TK expression CP_TK statement
		
|	IF_TK error
		
|	IF_TK OP_TK error
		
|	IF_TK OP_TK expression error
		
;

if_then_else_statement:
	IF_TK OP_TK expression CP_TK statement_nsi ELSE_TK statement
		
;

if_then_else_statement_nsi:
	IF_TK OP_TK expression CP_TK statement_nsi ELSE_TK statement_nsi
		
;

switch_statement:
	switch_expression
		
	switch_block
		
;

switch_expression:
	SWITCH_TK OP_TK expression CP_TK
		
|	SWITCH_TK error
		
|	SWITCH_TK OP_TK error
		
|	SWITCH_TK OP_TK expression CP_TK error
		
;

/* Default assignment is there to avoid type node on switch_block
   node. */

switch_block:
	OCB_TK CCB_TK
		
|	OCB_TK switch_labels CCB_TK
		
|	OCB_TK switch_block_statement_groups CCB_TK
		
|	OCB_TK switch_block_statement_groups switch_labels CCB_TK
		
;

switch_block_statement_groups:
	switch_block_statement_group
|	switch_block_statement_groups switch_block_statement_group
;

switch_block_statement_group:
	switch_labels block_statements
;

switch_labels:
	switch_label
|	switch_labels switch_label
;

switch_label:
	CASE_TK constant_expression REL_CL_TK
		
|	DEFAULT_TK REL_CL_TK
		
|	CASE_TK error
		
|	CASE_TK constant_expression error
		
|	DEFAULT_TK error
		
;

while_expression:
	WHILE_TK OP_TK expression CP_TK
		
;

while_statement:
	while_expression statement
		
|	WHILE_TK error
		
|	WHILE_TK OP_TK error
		
|	WHILE_TK OP_TK expression error
		
;

while_statement_nsi:
	while_expression statement_nsi
		
;

do_statement_begin:
	DO_TK
		
	/* Need error handing here. FIXME */
;

do_statement:
	do_statement_begin statement WHILE_TK OP_TK expression CP_TK SC_TK
		
;

for_statement:
	for_begin SC_TK expression SC_TK for_update CP_TK statement
		
|	for_begin SC_TK SC_TK for_update CP_TK statement
		
|	for_begin SC_TK error
		
|	for_begin SC_TK expression SC_TK error
		
|	for_begin SC_TK SC_TK error
		
;

for_statement_nsi:
	for_begin SC_TK expression SC_TK for_update CP_TK statement_nsi
		
|	for_begin SC_TK SC_TK for_update CP_TK statement_nsi
		
;

for_header:
	FOR_TK OP_TK
		
|	FOR_TK error
		
|	FOR_TK OP_TK error
		
;

for_begin:
	for_header for_init
		
;
for_init:			/* Can be empty */
		
|	statement_expression_list
		
|	local_variable_declaration
		
|	statement_expression_list error
		
;

for_update:			/* Can be empty */
		
|	statement_expression_list
		
;

statement_expression_list:
	statement_expression
		
|	statement_expression_list C_TK statement_expression
		
|	statement_expression_list C_TK error
		
;

break_statement:
	BREAK_TK SC_TK
		
|	BREAK_TK identifier SC_TK
		
|	BREAK_TK error
		
|	BREAK_TK identifier error
		
;

continue_statement:
	CONTINUE_TK SC_TK
		
|       CONTINUE_TK identifier SC_TK
		
|	CONTINUE_TK error
		
|	CONTINUE_TK identifier error
		
;

return_statement:
	RETURN_TK SC_TK
		
|	RETURN_TK expression SC_TK
		
|	RETURN_TK error
		
|	RETURN_TK expression error
		
;

throw_statement:
	THROW_TK expression SC_TK
		
|	THROW_TK error
		
|	THROW_TK expression error
		
;

assert_statement:
	ASSERT_TK expression REL_CL_TK expression SC_TK
		
|	ASSERT_TK expression SC_TK
		
|	ASSERT_TK error
		
|	ASSERT_TK expression error
		
;

synchronized_statement:
	synchronized OP_TK expression CP_TK block
		
|	synchronized OP_TK expression CP_TK error
		
|	synchronized error
		
|	synchronized OP_TK error CP_TK
		
|	synchronized OP_TK error
		
;

synchronized:
	modifiers
		
;

try_statement:
	TRY_TK block catches
		
|	TRY_TK block finally
		
|	TRY_TK block catches finally
		
|	TRY_TK error
		
;

catches:
	catch_clause
|	catches catch_clause
		
;

catch_clause:
	catch_clause_parameter block
		
;

catch_clause_parameter:
	CATCH_TK OP_TK formal_parameter CP_TK
		
|	CATCH_TK error
		
|	CATCH_TK OP_TK error
		
|	CATCH_TK OP_TK error CP_TK /* That's for () */
		
;

finally:
	FINALLY_TK block
		
|	FINALLY_TK error
		
;

/* 19.12 Production from 15: Expressions  */
primary:
	primary_no_new_array
|	array_creation_expression
;

primary_no_new_array:
	literal
|	THIS_TK
		
|	OP_TK expression CP_TK
		
|	class_instance_creation_expression
|	field_access
|	method_invocation
|	array_access
|	type_literals
        /* Added, JDK1.1 inner classes. Documentation is wrong
           refering to a 'ClassName' (class_name) rule that doesn't
           exist. Used name: instead.  */
|	name DOT_TK THIS_TK
		
|	OP_TK expression error
		
|	name DOT_TK error
		
|	primitive_type DOT_TK error
		
|	VOID_TK DOT_TK error
		
;

type_literals:
	name DOT_TK CLASS_TK
		
|	array_type DOT_TK CLASS_TK
		
|	primitive_type DOT_TK CLASS_TK
                
|	VOID_TK DOT_TK CLASS_TK
                
;

class_instance_creation_expression:
	NEW_TK class_type OP_TK argument_list CP_TK
		
|	NEW_TK class_type OP_TK CP_TK
		
|	anonymous_class_creation
        /* Added, JDK1.1 inner classes, modified to use name or
	   primary instead of primary solely which couldn't work in
	   all situations.  */
|	something_dot_new identifier OP_TK CP_TK
		
|	something_dot_new identifier OP_TK CP_TK class_body
|	something_dot_new identifier OP_TK argument_list CP_TK
		
|	something_dot_new identifier OP_TK argument_list CP_TK class_body
|	NEW_TK error SC_TK
		
|	NEW_TK class_type error
		
|	NEW_TK class_type OP_TK error
		
|	NEW_TK class_type OP_TK argument_list error
		
|	something_dot_new error
		
|	something_dot_new identifier error
		
;

/* Created after JDK1.1 rules originally added to
   class_instance_creation_expression, but modified to use
   'class_type' instead of 'TypeName' (type_name) which is mentionned
   in the documentation but doesn't exist. */

anonymous_class_creation:
	NEW_TK class_type OP_TK argument_list CP_TK
		
        class_body
		
|	NEW_TK class_type OP_TK CP_TK
		
        class_body
		
;

something_dot_new:		/* Added, not part of the specs. */
	name DOT_TK NEW_TK
		
|	primary DOT_TK NEW_TK
		
;

argument_list:
	expression
		
|	argument_list C_TK expression
		
|	argument_list C_TK error
		
;

array_creation_expression:
	NEW_TK primitive_type dim_exprs
		
|	NEW_TK class_or_interface_type dim_exprs
		
|	NEW_TK primitive_type dim_exprs dims
		
|	NEW_TK class_or_interface_type dim_exprs dims
		
        /* Added, JDK1.1 anonymous array. Initial documentation rule
           modified */
|	NEW_TK class_or_interface_type dims array_initializer
		
|	NEW_TK primitive_type dims array_initializer
		
|	NEW_TK error CSB_TK
		
|	NEW_TK error OSB_TK
		
;

dim_exprs:
	dim_expr
		
|	dim_exprs dim_expr
		
;

dim_expr:
	OSB_TK expression CSB_TK
		
|	OSB_TK expression error
		
|	OSB_TK error
		
;

dims:
	OSB_TK CSB_TK
		
|	dims OSB_TK CSB_TK
		
|	dims OSB_TK error
		
;

field_access:
	primary DOT_TK identifier
		
		/*  FIXME - REWRITE TO:
		 */
|	SUPER_TK DOT_TK identifier
		
|	SUPER_TK error
		
;

method_invocation:
	name OP_TK CP_TK
		
|	name OP_TK argument_list CP_TK
		
|	primary DOT_TK identifier OP_TK CP_TK
		
|	primary DOT_TK identifier OP_TK argument_list CP_TK
		
|	SUPER_TK DOT_TK identifier OP_TK CP_TK
		
|	SUPER_TK DOT_TK identifier OP_TK argument_list CP_TK
		
        /* Screws up thing. I let it here until I'm convinced it can
           be removed. FIXME
|	primary DOT_TK error
		{yyerror ("'(' expected"); DRECOVER(bad);} */
|	SUPER_TK DOT_TK error CP_TK
		
|	SUPER_TK DOT_TK error DOT_TK
		
;

array_access:
	name OSB_TK expression CSB_TK
		
|	primary_no_new_array OSB_TK expression CSB_TK
		
|	name OSB_TK error
		
|	name OSB_TK expression error
		
|	primary_no_new_array OSB_TK error
		
|	primary_no_new_array OSB_TK expression error
		
;

postfix_expression:
	primary
|	name
|	post_increment_expression
|	post_decrement_expression
;

post_increment_expression:
	postfix_expression INCR_TK
		
;

post_decrement_expression:
	postfix_expression DECR_TK
		
;

trap_overflow_corner_case:
	pre_increment_expression
|	pre_decrement_expression
|	PLUS_TK unary_expression
		
|	unary_expression_not_plus_minus
|	PLUS_TK error
		
;

unary_expression:
	trap_overflow_corner_case
		
|	MINUS_TK trap_overflow_corner_case
		
|	MINUS_TK error
		
;

pre_increment_expression:
	INCR_TK unary_expression
		
|	INCR_TK error
		
;

pre_decrement_expression:
	DECR_TK unary_expression
		
|	DECR_TK error
		
;

unary_expression_not_plus_minus:
	postfix_expression
|	NOT_TK unary_expression
		
|	NEG_TK unary_expression
 		
|	cast_expression
|       NOT_TK error
		
|       NEG_TK error
		
;

cast_expression:		/* Error handling here is potentially weak */
	OP_TK primitive_type dims CP_TK unary_expression
		
|	OP_TK primitive_type CP_TK unary_expression
		
|	OP_TK expression CP_TK unary_expression_not_plus_minus
		
|	OP_TK name dims CP_TK unary_expression_not_plus_minus
		
|	OP_TK primitive_type OSB_TK error
		
|       OP_TK error
		
|	OP_TK primitive_type dims CP_TK error
		
|	OP_TK primitive_type CP_TK error
		
|	OP_TK name dims CP_TK error
		
;

multiplicative_expression:
	unary_expression
|	multiplicative_expression MULT_TK unary_expression
		
|	multiplicative_expression DIV_TK unary_expression
		
|	multiplicative_expression REM_TK unary_expression
		
|	multiplicative_expression MULT_TK error
		
|	multiplicative_expression DIV_TK error
		
|	multiplicative_expression REM_TK error
		
;

additive_expression:
	multiplicative_expression
|	additive_expression PLUS_TK multiplicative_expression
		
|	additive_expression MINUS_TK multiplicative_expression
		
|	additive_expression PLUS_TK error
		
|	additive_expression MINUS_TK error
		
;

shift_expression:
	additive_expression
|	shift_expression LS_TK additive_expression
		
|	shift_expression SRS_TK additive_expression
		
|	shift_expression ZRS_TK additive_expression
		
|	shift_expression LS_TK error
		
|	shift_expression SRS_TK error
		
|	shift_expression ZRS_TK error
		
;

relational_expression:
	shift_expression
|	relational_expression LT_TK shift_expression
		
|	relational_expression GT_TK shift_expression
		
|	relational_expression LTE_TK shift_expression
		
|	relational_expression GTE_TK shift_expression
		
|	relational_expression INSTANCEOF_TK reference_type
		
|	relational_expression LT_TK error
		
|	relational_expression GT_TK error
		
|	relational_expression LTE_TK error
		
|	relational_expression GTE_TK error
		
|	relational_expression INSTANCEOF_TK error
		
;

equality_expression:
	relational_expression
|	equality_expression EQ_TK relational_expression
		
|	equality_expression NEQ_TK relational_expression
		
|	equality_expression EQ_TK error
		
|	equality_expression NEQ_TK error
		
;

and_expression:
	equality_expression
|	and_expression AND_TK equality_expression
		
|	and_expression AND_TK error
		
;

exclusive_or_expression:
	and_expression
|	exclusive_or_expression XOR_TK and_expression
		
|	exclusive_or_expression XOR_TK error
		
;

inclusive_or_expression:
	exclusive_or_expression
|	inclusive_or_expression OR_TK exclusive_or_expression
		
|	inclusive_or_expression OR_TK error
		
;

conditional_and_expression:
	inclusive_or_expression
|	conditional_and_expression BOOL_AND_TK inclusive_or_expression
		
|	conditional_and_expression BOOL_AND_TK error
		
;

conditional_or_expression:
	conditional_and_expression
|	conditional_or_expression BOOL_OR_TK conditional_and_expression
		
|	conditional_or_expression BOOL_OR_TK error
		
;

conditional_expression:		/* Error handling here is weak */
	conditional_or_expression
|	conditional_or_expression REL_QM_TK expression REL_CL_TK conditional_expression
		
|	conditional_or_expression REL_QM_TK REL_CL_TK error
		
|	conditional_or_expression REL_QM_TK error
		
|	conditional_or_expression REL_QM_TK expression REL_CL_TK error
		
;

assignment_expression:
	conditional_expression
|	assignment
;

assignment:
	left_hand_side assignment_operator assignment_expression
		
|	left_hand_side assignment_operator error
		
;

left_hand_side:
	name
|	field_access
|	array_access
;

assignment_operator:
	ASSIGN_ANY_TK
|	ASSIGN_TK
;

expression:
	assignment_expression
;

constant_expression:
	expression
;

%%

