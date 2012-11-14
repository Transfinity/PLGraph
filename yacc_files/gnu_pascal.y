/* Source: http://www.gnu-pascal.de/alpha/gpc-20050331-minimal.tar.bz2 */
/*A Bison parser for ISO 7185 Pascal, ISO 10206 Extended Pascal,
  Borland Pascal and some PXSC, Borland Delphi and GNU Pascal extensions.

  Copyright (C) 1989-2005 Free Software Foundation, Inc.

  Authors: Jukka Virtanen <jtv@hut.fi>
           Peter Gerwinski <peter@gerwinski.de>
           Frank Heckenbach <frank@pascal.gnu.de>
           Waldek Hebisch <hebisch@math.uni.wroc.pl>
           Bill Cox <bill@cygnus.com> (error recovery rules)

  This file is part of GNU Pascal.

  GNU Pascal is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published
  by the Free Software Foundation; either version 1, or (at your
  option) any later version.

  GNU Pascal is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Pascal; see the file COPYING. If not, write to the
  Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
  02111-1307, USA. */

/*Note: Bison automatically applies a default action of `$$ = $1' for all
  derivations; this is applied before the explicit action, if one is given.
  Keep this in mind when reading the actions.

  The error recovery strategy (the placement of `error' and `yyerrok') used
  here was that suggested in "An Introduction to Compiler Construction with
  Unix", by Axel Schreiner and H. George Friedman, chapter four, published by
  Prentice-Hall, 1985. Since it was done, the grammar has changed a lot, and now
  error recovery is more or less arbitrary (and possibly dangerous with GLR).

  For efficiency we build various lists in reverse order and `nreverse' them at
  the end which is O(n) instead of O(n^2).

  BP style typed constants and variables initialized with `=' would
  normally cause a shift/reduce conflict. They clash with upper-bound
  expressions of subranges. For an example, write

      const Foo: Boolean = False;
  as
      const Foo: False .. 1 = 1 = 2 = 7;

  (Nitpick: The types `Boolean' and `False .. True' are compatible, but
  not equivalent, in Pascal. But that's irrelevant to this problem.)

  This cannot be parsed (even BP can't do it). But consider the following:

      const Foo: False .. True = 2 = 7;
  or
      const Foo: False .. (1 = 1) = 2 = 7;

  This is resolved with a hack in the lexer: In this context, the `='
  immediately before the initializer is not lexed as `=' but as the special
  token LEX_CONST_EQUAL. */

%{
%}

%debug
%locations
%glr-parser
%no-default-prec
%expect 58
%expect-rr 23

/* The dangling `else' shift/reduce conflict is avoided by precedence rules.
   Also, some conflicts coming from error recovery rules are avoided by giving
   rules the precedence `lower_than_error'. To avoid unwanted effects we
   - declare `%no-default-prec'
   - don't use precedence for operators etc. where it can be avoided
   - use special `prec_...' symbols which are not used otherwise for rules */
%nonassoc prec_if prec_lower_than_error
%nonassoc p_else error

/* Keywords */
%token
  p_and p_array p_begin p_case p_div p_do p_downto p_end p_file p_for p_function
  p_goto p_if p_in p_label p_mod p_nil p_not p_of p_or p_packed p_procedure p_to
  p_program p_record p_repeat p_set p_then p_type p_until p_var p_while p_with

%token <ttype>
  p_absolute p_abstract p_and_then p_as p_asm p_attribute p_bindable p_const
  p_constructor p_destructor p_external p_far p_finalization p_forward
  p_implementation p_import p_inherited p_initialization p_is p_near p_object
  p_only p_operator p_otherwise p_or_else p_pow p_qualified p_restricted p_shl
  p_shr p_unit p_uses p_value p_virtual p_xor p_asmname p_c p_c_language

/* Built-in identifiers with special syntax */
%token <ttype>
  p_Addr p_Assigned p_Dispose p_Exit p_FormatString p_New p_Return

/* Lexer tokens */
%token <ttype>
  LEX_INTCONST LEX_INTCONST_BASE LEX_STRCONST LEX_REALCONST
  LEX_BUILTIN_PROCEDURE LEX_BUILTIN_PROCEDURE_WRITE LEX_BUILTIN_FUNCTION
  LEX_ID LEX_CARET_WHITE

%token <itype>
  LEX_CARET_LETTER

%token
  LEX_ASSIGN LEX_RENAME LEX_RANGE LEX_ELLIPSIS LEX_CONST_EQUAL
  LEX_SYMDIFF LEX_NE LEX_GE LEX_LE LEX_POWER LEX_BPPLUS LEX_BPMINUS
  LEX_CEIL_PLUS LEX_CEIL_MINUS LEX_FLOOR_PLUS LEX_FLOOR_MINUS
  LEX_CEIL_MULT LEX_CEIL_DIV LEX_FLOOR_MULT LEX_FLOOR_DIV

%%

/* Main program, module and unit structure */

program_component:
    /* empty */
      
  | program_component_1
      
  ;

program_component_1:
    optional_program_heading optional_module_attributes
      
    optional_import_part declarations_and_uses
      
    compound_statement dot_or_error
      
  | module_declaration
      
  ;

module_declaration:
    p_unit new_identifier ';' optional_module_attributes LEX_ID
      
    optional_import_part interface_decl_part
      
    optional_unit_implementation p_end dot_or_error
      
  | interface_module
  | implementatation_module
  | interface_module
      
    implementatation_module
      
  | module new_identifier optional_module_parameters
      
    module_interface ';'
      
    module_block dot_or_error
  | module new_identifier optional_module_parameters
      
    module_block dot_or_error
  ;

interface_module:
    module new_identifier LEX_ID
      
    optional_module_parameters
      
    module_interface dot_or_error
      
  ;

module_interface:
    LEX_ID
      
    export_part_list ';' optional_import_part interface_decl_part p_end
      
  ;

implementatation_module:
    module new_identifier p_implementation ';'
      
    module_block dot_or_error
  ;

module_block:
      
    optional_import_part1 implementation_decls
      
    optional_init_and_final_part p_end
  ;

optional_unit_implementation:
      
  | p_implementation optional_import_part1 declarations_and_uses optional_unit_init_and_final_part
  ;

optional_unit_init_and_final_part:
    p_begin
      
    rest_of_unit_constructor
  | unit_initialization
  | unit_initialization p_finalization
      
    statement_sequence
      
  | optional_init_and_final_part
  ;

unit_initialization:
    p_initialization
      
    rest_of_unit_constructor
  ;

rest_of_unit_constructor:
    statement_sequence
      
  ;

optional_init_and_final_part:
    /* empty */
  | module_constructor module_destructor
  | module_constructor
  | module_destructor
  ;

module_constructor:
    p_to p_begin p_do
      
    optional_statement ';'
      
  ;

module_destructor:
    p_to p_end p_do
      
    optional_statement ';'
      
  ;

optional_program_heading:
    /* empty */
      
  | p_program new_identifier optional_par_id_list ';'
      
  | p_program error optional_par_id_list ';'
      
  ;

optional_module_parameters:
    optional_par_id_list ';' optional_module_attributes
      
  ;

optional_par_id_list:
    null
  | '(' id_list ')'
      
  ;

optional_module_attributes:
    null
  | attributes ';'
  ;

export_part_list:
    export_part
  | export_part_list ';' export_part
      
  | error
      
  | export_part_list error export_part
      
  | export_part_list ';' error
      
  ;

export_part:
    new_identifier equals export_list_or_all
      
  ;

export_list_or_all:
    '(' export_list ')'
      
  | export_all
  | export_all '(' export_list ')'
      
  ;

export_all:
    LEX_ID
      
  ;

export_list:
    export_list_item
  | export_list ',' export_list_item
      
  | error
      
  | export_list error export_list_item
      
  | export_list ',' error
      
  ;

export_list_item:
    new_quid optional_rename
      
  | new_quid LEX_RANGE new_quid
      
  | i_protected new_quid optional_rename
      
  ;

optional_import_part:
      
    optional_import_part1
  ;

optional_import_part1:
    /* empty */
  | p_import import_specification_list ';'
  ;

import_specification_list:
    import_specification
  | import_specification_list ';' import_specification
  | import_specification_list error import_specification
      
  ;

import_specification:
    new_identifier optional_qualified optional_import_qualifier optional_unit_filename
      
  ;

optional_qualified:
    null
  | p_qualified
  ;

optional_import_qualifier:
    null
  | '(' import_clause_list ')'
      
  | p_only '(' import_clause_list ')'
      
  ;

import_clause_list:
    import_clause
  | import_clause_list ',' import_clause
      
  | error
      
  | import_clause_list error import_clause
      
  | import_clause_list ',' error
      
  ;

import_clause:
    new_identifier optional_rename
      
  ;

optional_rename:
    null
  | LEX_RENAME new_identifier
      
  ;

declarations_and_uses:
    /* empty */
      
  | declarations_and_uses declaration_or_uses
      
  ;

declaration_or_uses:
    uses_part
  | any_decl
  ;

any_declaration_part:
    /* empty */
      
  | any_declaration_part any_decl
      
  ;

any_decl:
      
    simple_decl
      
  | p_label label_list ';'
      
  | routine_declaration
      
  ;

interface_decl_part:
    /* empty */
      
  | interface_decl_part interface_decl
      
  ;

interface_decl:
    uses_part
  | simple_decl
  | routine_interface_decl
      
  ;

implementation_decls:
    /* empty */
      
  | implementation_decls implementation_decl
      
  ;

implementation_decl:
    uses_part
  |   
    simple_decl
      
  | routine_declaration
      
  ;

uses_part:
    p_uses uses_list ';'
      
  ;

uses_list:
    uses_specification
  | uses_list ',' uses_specification
  | uses_list error uses_specification
      
  ;

uses_specification:
    new_identifier optional_unit_filename
      
  ;

optional_unit_filename:
    null
  | p_in expression
      
  ;

simple_decl:
      
    simple_decl_1
      
  ;

simple_decl_1:
    p_const constant_definition_list
      
  | p_type
      
    type_definition_list ';'
      
  | p_var variable_declaration_list
      
  ;

/* Label declaration part */

label_list:
    new_label
      
  | label_list ',' new_label
      
  | error
      
  | label_list error new_label
      
  | label_list ',' error
      
  | label_list error
  ;

new_label:
    num_label
  | new_identifier
      
  ;

constant_definition_list:
    constant_definition
  | constant_definition_list constant_definition
  | error
  ;

constant_definition:
    new_identifier equals static_expression ';'
      
  | new_identifier enable_lce ':' type_denoter_with_attributes
    LEX_CONST_EQUAL component_value optional_variable_directive_list_no_external ';'
      
  ;

variable_declaration_list:
    variable_declaration
  | variable_declaration_list variable_declaration
  ;

variable_declaration:
    id_list_limited enable_lce ':' type_denoter_with_attributes
      
    absolute_or_value_specification optional_variable_directive_list ';'
      
  | error
      
  ;

optional_variable_directive_list:
    null
  | optional_variable_directive_list ';' variable_directive
      
  ;

variable_directive:
    variable_directive_no_external
  | p_external optional_combined_string
      
  | p_external optional_combined_string LEX_ID
      
    expression
      
  ;

optional_variable_directive_list_no_external:
    null
  | optional_variable_directive_list_no_external ';' variable_directive_no_external
      
  ;

variable_directive_no_external:
    p_asmname expression
      
  | attributes
      
  ;

absolute_or_value_specification:
    optional_value_specification
  | p_absolute
      
    expression
      
  | p_absolute error
      
  ;

type_definition_list:
    type_definition
  | type_definition_list ';' type_definition
      
  | error
  | type_definition_list error type_definition
      
  | type_definition_list ';' error
      
  | type_definition_list error
  ;

type_definition:
    new_identifier enable_lce equals type_denoter_with_attributes optional_value_specification
      
  | new_identifier formal_schema_discriminants equals
      
    type_denoter_with_attributes optional_value_specification
      
  | new_identifier formal_schema_discriminants error
      
  | new_identifier enable_lce equals
      
    optional_abstract p_object object_parent object_field_list p_end
      
  ;

formal_schema_discriminants:
    '(' discriminant_specification_list ')'
      
  | '(' error ')'
      
  ;

discriminant_specification_list:
    discriminant_specification
  | discriminant_specification_list ';' discriminant_specification
      
  | discriminant_specification_list error discriminant_specification
      
  | discriminant_specification_list ';' error
      
  | discriminant_specification_list error
  ;

discriminant_specification:
    id_list ':' typename
      
  ;

type_denoter_with_attributes:
    type_denoter
  | type_denoter_with_attributes attributes
      
  ;

/* Ensure that obstack change is active for all parts of type denoters. */
type_denoter:
      
    type_denoter_1
      
  ;

type_denoter_1:
    type_denoter_0
  | p_bindable type_denoter_0
      
  ;

type_denoter_0:
    typename_or_string255
  | p_restricted typename_or_string255
      
  | typename actual_schema_discriminants
      
  | type_inquiry
  | new_ordinal_type
  | new_pointer_type
  | new_procedural_type
      
  | unpacked_structured_type
  | packed unpacked_structured_type
      
  ;

actual_schema_discriminants:
    '(' discriminant_expression_list ')'
      
  | '[' expression ']'
      
  ;

discriminant_expression_list:
    expression
      /* This expression might be a discriminant of another schema. */
      
  | discriminant_expression_list ',' expression
      
  | error
      
  | discriminant_expression_list error expression
      
  | discriminant_expression_list ',' error
      
  ;

unpacked_structured_type:
    p_array '[' array_index_list ']' p_of type_denoter
      
  | untyped_file
  | p_file direct_access_index_type p_of type_denoter
      
  | p_set p_of type_denoter
      
  | p_record record_field_list p_end
      
  | p_record error p_end
      
  | err
  ;

direct_access_index_type:
    null
  | '[' ordinal_index_type ']'
      
  ;

array_index_list:
    ordinal_index_type
  | array_index_list ',' ordinal_index_type
      
  | array_index_list error ordinal_index_type
      
  | array_index_list error
      
  | error
      
  ;

ordinal_index_type:
    new_ordinal_type
      
  | typename
      
  ;

record_field_list:
    /* empty */
      
  | fixed_part optional_semicolon
      
  | optional_fixed_part p_case variant_selector p_of variant_list optional_semicolon rest_of_variant
      
  ;

optional_fixed_part:
    null
  | fixed_part ';'
  ;

fixed_part:
    record_section
  | fixed_part ';' record_section
      
  | fixed_part error record_section
      
  | fixed_part ';' error
      
  | fixed_part error
  ;

record_section:
    id_list ':' type_denoter optional_value_specification
      
  ;

rest_of_variant:
    /* empty */
      
  | otherwise '(' record_field_list ')' optional_semicolon
      
  ;

variant_selector:
    new_identifier ':' typename
      
  | new_identifier ':' new_ordinal_type_non_iso
      
  | id  /* type name or discriminant identifier! */
      
  | new_ordinal_type_non_iso
      
  ;

variant_list:
    variant
  | variant_list ';' variant
      
  | variant_list error variant
      
  | error
      
  | variant_list error
  ;

variant:
    case_constant_list ':' '(' record_field_list ')'
      
  ;

new_ordinal_type_non_iso:
    new_ordinal_type
      
  ;

new_ordinal_type:
    enumerated_type
  | subrange_type
  ;

enumerated_type:
    '(' enum_list ')'
      
  | '(' error ')'
      
  ;

enum_list:
    new_identifier
      
  | enum_list ',' new_identifier
      
  | enum_list error new_identifier
      
  | enum_list ',' error
      
  | enum_list error
  ;

subrange_type:
    expression LEX_RANGE expression
      
  | packed expression LEX_RANGE expression
      
  ;

new_pointer_type:
    '@' pointer_domain_type
      
  | '^' pointer_domain_type
      
  | LEX_CARET_WHITE pointer_domain_type
      
  | pointer_char p_const pointer_domain_type
      
  ;

pointer_domain_type:
    new_identifier
      
  | new_procedural_type
      
  | untyped_file
  ;

new_procedural_type:
    p_procedure optional_formal_parameter_list
      
  | p_function optional_formal_parameter_list resulttype
      
  ;

object_parent:
    null
  | '(' typename ')'
      
  | '(' error ')'
      
  ;

optional_abstract:
    null
  | p_abstract
  ;

object_field_list:
    object_field_list_1
  | object_field_list_1 object_section
      
  | error
      
  ;

object_field_list_1:
    null
  | object_field_list_1 LEX_ID  /* `public' etc. */
      
  | object_field_list_1 object_section ';'
      
  ;

object_section:
    id_list ':' type_denoter optional_value_specification
      
  | p_procedure new_identifier optional_formal_parameter_list
      
  | p_function new_identifier optional_formal_parameter_list optional_result_def resulttype
      
  | p_constructor new_identifier optional_formal_parameter_list
      
  | p_destructor new_identifier optional_formal_parameter_list
      
  | p_virtual
      
  | p_virtual expression
      
  | p_abstract
      
  | attributes
  ;

optional_value_specification:
    null
  | var_init component_value
      
  | var_init error
      
  ;

var_init:
    p_value 
  | LEX_ASSIGN
      
  | LEX_CONST_EQUAL
      
  ;

/* Routines (procedures represented as functions returning void_type_node) */

routine_interface_decl:
    routine_heading ';' optional_routine_interface_directive_list
      
  ;

/* Distinguish between
     procedure Foo; <attributes> <remote_directive> <attributes>
   and
     procedure Bar; <attributes>
     begin
     end; */
routine_declaration:
    routine_or_method_heading ';' remote_directive_list
      
  | routine_or_method_heading ';' optional_routine_directive_list
      
    optional_import_part1 any_declaration_part
      
    compound_statement ';'
      
  ;

routine_or_method_heading:
    routine_heading
  | method_heading
  ;

routine_heading:
    p_procedure new_identifier optional_formal_parameter_list
      
  | p_function new_identifier optional_formal_parameter_list optional_result_def resulttype
      
  | p_operator operator_identifier optional_formal_parameter_list operator_result_def resulttype
      
  ;

/* Using identifier instead of new_identifier for the object type would cause
   conflicts with non-method routines and (in the future) accept qualified
   identifiers (which are pointless in the header of an object method
   implementation). With new_identifier we only have to check
   (in build_routine_heading) that the object type really exists. */
method_heading:
    p_procedure new_identifier '.' new_identifier optional_formal_parameter_list
      
  | p_function new_identifier '.' new_identifier optional_formal_parameter_list optional_result_def resulttype
      
  | p_constructor new_identifier '.' new_identifier optional_formal_parameter_list
      
  | p_destructor new_identifier '.' new_identifier optional_formal_parameter_list
      
  ;

optional_routine_interface_directive_list:
    optional_routine_directive_list
  | remote_directive_list
  ;

remote_directive_list:
    optional_routine_directive_list remote_directive optional_routine_directive_list
      
  ;

remote_directive:
    p_forward ';'
      
  | p_external optional_combined_string ';'
      
  | p_external optional_combined_string LEX_ID
      
    expression ';'
      
  | p_external optional_combined_string ';' p_asmname expression ';'
      
  | p_asmname expression ';' p_external optional_combined_string ';'
      
  | p_asmname expression ';'
      
  | p_c ';'
      
  | p_c_language ';'
      
  ;

optional_routine_directive_list:
    null
  | optional_routine_directive_list attributes ';'
      
  | optional_routine_directive_list bp_directive ';'
      
  ;

attributes:
    p_attribute '(' attribute_list ')'
      
  ;

attribute_list:
    attrib
  | attribute_list ',' attrib
      
  ;

attrib:
    null
  | p_const
      
  | new_identifier
      
  | new_identifier equals expression
      
  | new_identifier '(' expression ')'
      
  | new_identifier '(' id ',' expression ')'
      
  ;

bp_directive:
    p_far
  | p_near
  ;

operator_identifier:
    new_identifier
  | operator_symbol
  ;

operator_result_def:
    new_identifier
  | equals new_identifier
      
  | null
      
  ;

optional_formal_parameter_list:
    null
  | '(' ')'
      
  | '(' formal_parameter_list ')'
      
  | '(' LEX_ELLIPSIS ')'
      
  | '(' formal_parameter_list ';' LEX_ELLIPSIS ')'
      
  | '(' error ')'
      
  ;

formal_parameter_list:
    formal_parameter
  | formal_parameter_list ';' formal_parameter
      
  | formal_parameter_list error formal_parameter
      
  | formal_parameter_list ';' error
  ;

formal_parameter:
    id_list ':' parameter_form
      
  | i_protected id_list ':' parameter_form
      
  | optional_protected p_var id_list optional_parameter_form
      
  | p_const id_list optional_parameter_form
      
  | p_procedure new_identifier optional_formal_parameter_list
      
  | p_function new_identifier optional_formal_parameter_list optional_result_def resulttype
      
  ;

resulttype:
    empty_lte
      
  | ':' typename_or_string255
      
  | err
  ;

optional_result_def:
    null
  | optional_result_equals new_identifier
      
  ;

optional_result_equals:
    equals
  | /* empty */
      
  ;

optional_protected:
    null
  | i_protected
  ;

optional_parameter_form:
    /* empty */
      
  | ':' parameter_form
      
  ;

parameter_form:
    typename_or_conformant_array
  | type_inquiry
  | open_array
  ;

type_inquiry:
    p_type p_of expression
      
  ;

typename_or_conformant_array:
    typename_or_untyped_file
  | conformant_array
  ;

conformant_array:
    p_array '[' index_type_specification_list ']' p_of typename_or_conformant_array
      
  | packed p_array '[' index_type_specification ']' p_of typename
      
  ;

index_type_specification_list:
    index_type_specification
  | index_type_specification_list ';' index_type_specification
      
  | index_type_specification_list error index_type_specification
      
  | index_type_specification_list ';' error
  ;

index_type_specification:
    new_identifier LEX_RANGE new_identifier ':' typename
      
  ;

open_array:
    p_array p_of typename_or_untyped_file
      
  ;

typename_or_untyped_file:
    typename
  | untyped_file
  ;

/* Statements */

compound_statement:
    p_begin pushlevel statement_sequence poplevel p_end
      
  ;

statement_sequence:
    optional_statement_vd
  | statement_sequence ';' optional_statement_vd
      
  ;

optional_statement_vd:
    /* empty */
  | statement
  | p_var
      
    variable_declaration
      
    optional_statement_vd
  ;

optional_statement:
    empty_lte
      
  | statement
      
  ;

empty_lte:
    %prec prec_lower_than_error
  ;

statement:
      
    statement_1
      
  ;

statement_1:
    set_label
  | set_label unlabelled_statement
  | unlabelled_statement
  ;

/* The condition of `repeat' and `while' loops must be within the pushlevel
   in case it creates temporary variables (fjf419b.pas). */

unlabelled_statement:
    compound_statement
  | start_of_assignment_or_call
      
  | start_of_assignment_or_call
      
    assign expression
      
  | p_Return
      
  | p_Return expression
      
  | p_Exit
      
  | p_Exit '(' p_program ')'
      
  | p_Exit '(' id ')'
      
  | builtin_procedure_statement
  | p_with with_list p_do pushlevel optional_statement poplevel
      
  | if_then %prec prec_if
      
  | if_then p_else
      
    pushlevel optional_statement poplevel
      
  | p_case expression p_of
      
    optional_case_element_list
      
    optional_case_completer p_end
      
  | p_repeat
      
    pushlevel statement_sequence p_until
      
    expression
      
    poplevel
      
  | p_while
      
    pushlevel expression
      
    p_do optional_statement poplevel
      
  | p_for variable_or_routine_access assign expression for_direction expression
      
    p_do pushlevel optional_statement poplevel
      
  | p_for variable_or_routine_access p_in expression
      
    p_do pushlevel optional_statement poplevel
      
  | p_goto label
      
  ;

set_label:
    label ':'
      
  ;

with_list:
    with_variable
  | with_list ',' with_variable
      
  | error
      
  | with_list error with_variable
      
  | with_list ',' error
      
  ;

with_variable:
    expression
      
  | expression ':' new_identifier
      
  ;

if_then:
    p_if expression p_then
      
    pushlevel optional_statement poplevel
      
  ;

optional_case_completer:
    /* empty */
      
  | otherwise pushlevel statement_sequence poplevel
      
  ;

otherwise:
    p_else
      
  | p_otherwise
  ;

optional_case_element_list:
    /* empty */
      
  | case_element_list optional_semicolon
      
  ;

case_element_list:
    case_element
  | case_element_list ';' case_element
      
  | error
      
  | case_element_list error case_element
      
  | case_element_list ';' error
      
  ;

case_element:
    case_constant_list ':'
      
    pushlevel optional_statement poplevel
      
  ;

case_constant_list:
    one_case_constant
  | case_constant_list ',' one_case_constant
      
  | case_constant_list ',' error
      
  | case_constant_list error one_case_constant
      
  | case_constant_list error
  ;

one_case_constant:
    static_expression %prec prec_lower_than_error
      
  | static_expression LEX_RANGE static_expression
      
  | static_expression error static_expression
      
  | static_expression LEX_RANGE error
      
  ;

for_direction:
    p_to
      
  | p_downto
      
  | error
      
  ;

start_of_assignment_or_call:
    variable_or_routine_access
  | '@' variable_or_routine_access
      
  ;

assign:
    LEX_ASSIGN
  | equals
      
  ;

builtin_procedure_statement:
    builtin_proc builtin_actual_parameter_list
      
  | LEX_BUILTIN_PROCEDURE_WRITE
      
  | LEX_BUILTIN_PROCEDURE_WRITE '(' write_actual_parameter_list ')'
      
  | p_Dispose '(' expression ')'
      
  | p_Dispose '(' expression ',' actual_parameter_list ')' %dprec 1
      
  | p_Dispose '(' expression ',' new_identifier builtin_actual_parameter_list ')' %dprec 2
      
  | p_asm asm_qualifier '(' string_constants ')'
      
  | p_asm asm_qualifier '(' string_constants ':' asm_operands ')'
      
  | p_asm asm_qualifier '(' string_constants ':' asm_operands ':' asm_operands ')'
      
  | p_asm asm_qualifier '(' string_constants ':' asm_operands ':' asm_operands ':' asm_clobbers ')'
      
  ;

asm_qualifier:
    /* empty */
      
  | LEX_ID
      
  ;

asm_operands:
    null
  | nonempty_asm_operands
  ;

nonempty_asm_operands:
    asm_operand
  | nonempty_asm_operands ',' asm_operand
      
  ;

asm_operand:
    combined_string '(' expression ')'
      
  ;

asm_clobbers:
    combined_string
      
  | asm_clobbers ',' combined_string
      
  ;

/* Expressions */

static_expression:
    expression
      
  ;

expression:
    simple_expression
      
  | simple_expression relational_operator simple_expression
      
  ;

simple_expression:
    term
  | sign term
      
  | simple_expression adding_operator term
      
  | simple_expression pxsc_adding_operator term
      
  | simple_expression or_operator
      
    term
      
  ;

term:
    factor
  | term multiplying_operator factor
      
  | term pxsc_multiplying_operator factor
      
  | term and_operator
      
    factor
      
  ;

factor:
    primary
  | factor id primary
      
  | primary p_pow primary
      
  | primary LEX_POWER primary
      
  | factor p_is typename
      
  | factor p_as typename
      
  ;

primary:
    unary_operator primary
      
  | '@' primary
      
  | combined_string
  | unsigned_number
  | p_nil
      
  | set_constructor
  | variable_or_routine_access
      
  ;

unsigned_number:
    LEX_INTCONST
  | LEX_INTCONST_BASE
  | LEX_REALCONST
  ;

optional_combined_string:
    null
  | combined_string
  ;

combined_string:
    string_constants
      
  ;

string_constants:
    string_constant
      
  | string_constants string_constant
      
  ;

string_constant:
    LEX_STRCONST
  | LEX_CARET_WHITE
  | '^' caret_chars
      
  ;

caret_chars:
    LEX_CARET_LETTER
  | ',' | '.' | ':' | ';' | '(' | ')' | '[' | ']'
  | '+' | '-' | '*' | '/' | '<' | '=' | '>' | '@' | '^'
  ;

typename_or_string255:
    typename
      
  ;

typename:
    typename_1
      
  ;

typename_1:
    id
  | id '.' id
      
  ;

variable_or_routine_access:
    LEX_BUILTIN_FUNCTION builtin_actual_parameter_list
      
  | variable_or_routine_access_no_builtin_function
  ;

variable_or_routine_access_no_builtin_function:
    identifier
  | untyped_file
      
  | '(' expression ')'
      
  | variable_or_routine_access '.' new_identifier
      
  | variable_or_routine_access pointer_char
      
  | variable_or_routine_access '[' index_expression_list ']' %dprec 1
      
  | variable_or_routine_access '[' structured_constructor_list ']' %dprec 2
      
  | variable_or_routine_access_no_builtin_function '(' ')'
      
  | variable_or_routine_access_no_builtin_function '(' disable_function_calls actual_parameter_list ')'
      
  | p_inherited new_identifier
      
  | p_FormatString '(' write_actual_parameter_list ')'
      
  | p_Assigned '(' disable_function_calls expression ')'
      
  | p_Addr '(' variable_or_routine_access ')'
      
  | p_New '(' variable_or_routine_access ')'
      
  | p_New '(' variable_or_routine_access ',' actual_parameter_list ')' %dprec 1
      
  | p_New '(' variable_or_routine_access ',' new_identifier builtin_actual_parameter_list ')' %dprec 2
      
  ;

builtin_actual_parameter_list:
    null
  | '(' disable_function_calls actual_parameter_list ')' %dprec 1
      
  | '(' disable_function_calls variable_or_routine_access ')' %dprec 2
      
  ;

disable_function_calls:
      
  ;

actual_parameter_list:
    expression
      
  | actual_parameter_list ',' expression
      
  | error
      
  | actual_parameter_list ',' error
      
  ;

write_actual_parameter_list:
    write_actual_parameter
  | write_actual_parameter_list ',' write_actual_parameter
      
  | write_actual_parameter_list error write_actual_parameter
      
  | write_actual_parameter_list ',' error
      
  ;

/* `:' specifiers are represented as a list in TREE_PURPOSE of each actual parameter. */
write_actual_parameter:
    expression
      
  | expression ':' expression
      
  | expression ':' expression ':' expression
      
  ;

untyped_file:
    p_file
      
  ;

structured_constructor_list:
    field_value_list optional_semicolon
  | field_value_list ';' variant_part_value
      
  | field_value_list optional_semicolon array_value_completer
      
  | variant_part_value
  | array_value_completer
  ;

field_value_list:
    field_value
  | field_value_list ';' field_value
      
  ;

field_value:
    index_expression_list ':' component_value
      
  | component_value
      
  ;

variant_part_value:
    p_case new_identifier ':' expression p_of variant_part_value0
      
  | p_case expression p_of variant_part_value0
      
  ;

variant_part_value0:
    '[' structured_constructor_list ']' optional_semicolon
      
  ;

array_value_completer:
    p_otherwise component_value optional_semicolon
      
  ;

component_value:
    expression %dprec 1
      
  | '(' ')'
      
  | '(' bp_constructor_list ')' %dprec 2
      
  | '[' structured_constructor_list ']' %dprec 2
      
  | '[' error ']'
      
  ;

bp_constructor_list:
    bp_field_value
  | bp_constructor_list initializer_separator bp_field_value
      
  | bp_constructor_list error bp_field_value
      
  ;

initializer_separator:
    ';'
  | ','
      
  ;

bp_field_value:
    index_expression_item ':' component_value
      
  | component_value
      
  ;

index_expression_list:
    index_expression_item
  | index_expression_list ',' index_expression_item
      
  | error
      
  | index_expression_list error index_expression_item
      
  | index_expression_list ',' error
      
  ;

index_expression_item:
    new_identifier %dprec 2
      
  | expression %dprec 1
      
  | expression LEX_RANGE expression
      
  ;

set_constructor:
    '[' ']'
      
  | '[' set_constructor_element_list ']'
      
  ;

set_constructor_element_list:
    member_designator
  | set_constructor_element_list ',' member_designator
      
  | set_constructor_element_list error member_designator
      
  | set_constructor_element_list ',' error
      
  ;

member_designator:
    expression
      
  | expression LEX_RANGE expression
      
  ;

/* Operators */

sign:
    '+' 
  | '-' 
  ;

unary_operator:
    LEX_BPPLUS  
  | LEX_BPMINUS 
  | p_not       
  ;

relational_operator:
    LEX_NE 
  | LEX_LE 
  | LEX_GE 
  | '='    
  | '<'    
  | '>'    
  | p_in   
  ;

adding_operator:
    '+'         
  | LEX_BPPLUS  
  | '-'         
  | LEX_BPMINUS 
  | p_xor       
  | LEX_SYMDIFF 
  ;

multiplying_operator:
    '*'   
  | '/'   
  | p_div 
  | p_mod 
  | p_shl 
  | p_shr 
  ;

or_operator:
    p_or        
  | p_or p_else 
  | p_or_else   
  | '|'         
  ;

and_operator:
    p_and        
  | p_and p_then 
  | p_and_then   
  | '&'          
  ;

builtin_proc:
    LEX_BUILTIN_PROCEDURE
      
  /* operators used as "procedures" */
  | p_and 
  | p_or  
  | p_not 
  | p_xor 
  | p_shl 
  | p_shr 
  ;

/* Don't use identifiers like `Plus' for symbols which would set a
   strange spelling for a possible such user-defined identifier. */
operator_symbol:
    '+'         
  | LEX_BPPLUS  
  | '-'         
  | LEX_BPMINUS 
  | '*'         
  | '/'         
  | p_div       
  | p_mod       
  | LEX_POWER   
  | p_in        
  | '<'         
  | equals      
  | '>'         
  | LEX_NE      
  | LEX_GE      
  | LEX_LE      
  | p_and       
  | '&'         
  | p_or        
  | '|'         
  | LEX_SYMDIFF 
  | pxsc_multiplying_operator
  | pxsc_adding_operator
  ;

pxsc_adding_operator:
    LEX_CEIL_PLUS   
  | LEX_CEIL_MINUS  
  | LEX_FLOOR_PLUS  
  | LEX_FLOOR_MINUS 
  ;

pxsc_multiplying_operator:
    LEX_CEIL_MULT  
  | LEX_CEIL_DIV   
  | LEX_FLOOR_MULT 
  | LEX_FLOOR_DIV  
  ;

/* To avoid parse conflicts with `otherwise'. No need to check the dialect here,
   this happens already when the label is declared. */
label:
    num_label
  | id
  ;

num_label:
    LEX_INTCONST
      
  ;

id_list:
    id_list1 %prec prec_lower_than_error
      
  ;

id_list1:
    new_identifier
      
  | id_list1 ',' new_identifier
      
  | id_list1 error new_identifier
      
  | id_list1 ',' error
      
  | id_list1 error
  ;

id_list_limited:
    new_identifier
      
  | id_list_limited ',' new_identifier
      
  ;

new_quid:
    new_identifier
  | new_identifier '.' new_identifier
      
  ;

new_identifier:
    new_identifier_1
      
  ;

new_identifier_1:
    id
  | LEX_BUILTIN_PROCEDURE
  | LEX_BUILTIN_PROCEDURE_WRITE
  | LEX_BUILTIN_FUNCTION
  | p_Return
  | p_Exit
  | p_Addr
  | p_Assigned
  | p_New
  | p_Dispose
  | p_FormatString
  | p_absolute
  | p_abstract
  | p_and_then
  | p_as
  | p_asm
  | p_asmname
  | p_attribute
  | p_bindable
  | p_c
  | p_c_language
  | p_constructor
  | p_destructor
  | p_external
  | p_finalization
  | p_implementation
  | p_import
  | p_inherited
  | p_initialization
  | p_is
  | p_object
  | p_only
  | p_operator
  | p_or_else
  | p_otherwise
  | p_pow
  | p_qualified
  | p_restricted
  | p_shl
  | p_shr
  | p_unit
  | p_uses
  | p_value
  | p_virtual
  | p_xor
  ;

identifier:
    id
      
  ;

id:
    LEX_ID
  | caret_letter
  | p_far
  | p_forward
  | p_near
  ;

caret_letter:
    LEX_CARET_LETTER
      
  ;

/* Support states */

pushlevel:
    /* empty */
      
  ;

poplevel:
    /* empty */
      
  ;

enable_lce:
    /* empty */
      
  ;

packed:
    p_packed
      
  ;

i_protected:
    LEX_ID
      
  ;

module:
    LEX_ID
      
  ;

optional_semicolon:
    /* empty */
      
  | ';'
      
  ;

pointer_char:
    '^'
  | LEX_CARET_WHITE
  | '@'
  ;

equals:
    '='
  | LEX_CONST_EQUAL
  ;

dot_or_error:
    '.'
  | error
      
  ;

null:
    /* empty */
      
  ;

err:
    error
      
  ;

%%
