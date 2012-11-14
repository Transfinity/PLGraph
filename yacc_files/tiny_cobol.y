/*
 * Copyright (C) 2007-2010 Rildo Pragana, David Essex. 
 * Copyright (C) 1999-2006 Rildo Pragana,
 *               Bernard Giroud, David Essex, Jim Noeth.
 * Copyright (C) 1991,1993 Rildo Pragana.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA 

 *
 * TinyCOBOL parser 
 *

*/

%{
%}

/* %nonassoc LOW_PREC */

%token <str>  IDSTRING_WORD IDSTRING_LITERAL
/* %token <strty>  IDSTRING_WORD IDSTRING_LITERAL */
%token <sval> STRING VARIABLE VARCOND SUBSCVAR
%token <sval> LABELSTR CMD_LINE ENVIRONMENT_VARIABLE INKEY
%token <ival> CHAR MULTIPLIER
%token <ival> USAGENUM CONDITIONAL
%token <ival> TO IS ARE THRU THAN NO
%token <ival> DIRECTION READ WRITE INPUT_OUTPUT RELEASE
%token <lval> NLITERAL CLITERAL
%token <ival> DISK PRINTER DATE_TIME

%left   '+' '-'
%left   '*' '/'
%left   POW_OP

%left  OR
%left  AND
%right NOT

%token ACCEPT ACCESS ADD ADDRESS ADVANCING AFTER ALL ALPHABET
%token ALPHABETIC_TOK ALPHABETIC_LOWER ALPHABETIC_UPPER ALPHANUMERIC
%token ALPHANUMERIC_EDITED ALSO ALTERNATE ANY AREA AREAS ASSIGN
%token AT AUTHOR AUTO 
%token BACKGROUNDCOLOR BEFORE BELL BLANK BLINK BLOCK BOTTOM BY
%token CALL CALL_CONV_C CALL_CONV_STDCALL CALL_LOADLIB CANCEL CENTER CF CH 
%token CHAIN CHAINING CHARACTER CHARACTERS CLASS CLOSE 
%token CODE CODE_SET COLLATING COLOR COLUMN COLUMNS COMMA COMMON COMPUTE CONFIGURATION
%token CONSOLE CONTAINS CONTENT CONTINUE CONTROL CONTROLS CONVERTING
%token CORRESPONDING COUNT CURRENCY CURSOR
%token DATA DATE_COMPILED DATE_WRITTEN DE DEBUGGING DECIMAL_POINT 
%token DECLARATIVES DELETE DELIMITED DELIMITER DEPENDING DETAIL DISPLAY
%token DISPLAY_SCREEN DIVIDE DIVISION DOWN DUPLICATES DYNAMIC
%token ELSE END END_ACCEPT END_ADD END_CALL END_CALL_LOADLIB END_CHAIN END_COMPUTE END_DELETE
%token END_DISPLAY END_DIVIDE END_EVALUATE END_IF END_MULTIPLY END_OF_PAGE
%token END_PERFORM END_READ END_RETURN END_REWRITE END_SEARCH END_START
%token END_STRINGCMD END_SUBTRACT END_UNSTRING END_WRITE ENVIRONMENT
%token EOL EOS ERASE ERROR_TOK EVALUATE EXCEPTION EXIT EXTEND
%token EXTERNAL
%token FALSE_TOK FD FILE_CONTROL FILE_ID FILE_TOK FILLER FINAL FIRST
%token FOOTING FOR FOREGROUNDCOLOR FROM FULL FUNCTION
%token GENERATE GIVING GLOBAL GO GOBACK GROUP
%token HEADING HIGHLIGHT HIGHVALUES
%token IDENTIFICATION IF IGNORE IN INDEXED INDICATE INITIALIZE INITIAL_TOK
%token INITIATE INPUT INSPECT INSTALLATION INTO INVALID I_O I_O_CONTROL
%token JUSTIFIED
%token KEY
%token LABEL LAST LEADING LEFT LENGTH LIMIT LIMITS LINAGE LINE LINES LINKAGE LISTSEP
%token LOCK LOWER LOWLIGHT LOWVALUES LPAR
%token MERGE MINUS MODE MOVE MULTIPLE MULTIPLY
%token NATIVE NEGATIVE NEXT NOECHO NOT NOTEXCEP NULL_TOK NUMBER NUMBERS
%token NUMERIC NUMERIC_EDITED
%token OBJECT_COMPUTER OCCURS OF OFF OMITTED ON ONLY OPEN OPTIONAL ORDER
%token ORGANIZATION OTHER OUTPUT OVERFLOW_TOK
%token PADDING PAGE_TOK PAGE_COUNTER PARAGRAPH PERFORM PF PH PICTURE PLUS POINTER POSITION
%token POSITIVE PREVIOUS PROCEDURE PROCEDURES PROCEED PROGRAM PROGRAM_ID
%token QUOTES
%token RANDOM RD READY RECORD RECORDS REDEFINES REEL REFERENCE REFERENCES RELATIVE
%token REMAINDER REMOVAL RENAMES REPLACING REPORT REPORTING REPORTS REQUIRED RESERVE RESET
%token RETURN_TOK RETURNING REVERSEVIDEO REWIND REWRITE RF RH RIGHT ROUNDED RUN
%token SAME SCREEN SD SEARCH SECTION SECURE SECURITY SELECT SENTENCE SEPARATE
%token SEQUENCE SEQUENTIAL SET SIGN SIZE SORT SORT_MERGE SOURCE SOURCE_COMPUTER
%token SPACES SPECIAL_NAMES STANDARD STANDARD_1 STANDARD_2 START STATUS
%token STD_ERROR STD_INPUT STD_OUTPUT STOP STRINGCMD SUBTRACT SUM SYNCHRONIZED
%token TALLYING TAPE TCOBPROTO1 TCOBPROTO2 TERMINATE TEST THEN TIMES TOKDUMMY
%token TOP TRACE TRAILING TRUE_TOK TYPE
%token UNDERLINE UNIT UNLOCK UNSTRING UNTIL UP UPDATE UPON UPPER USAGE USE USING
%token VALUE VALUES VARYING
%token WHEN WITH WORKING_STORAGE
%token ZERO

%right OF
%nonassoc PERIOD_TOK

%%

/************   Parser for Cobol Source  **************/
/*
   COMPILATION UNIT STRUCTURE
*/
root_: program_sequences 
        ;
program_sequences:
        program 
        | program_sequence 
        | program_sequence program 
/*
          program_sequence program_sequence end_program_opt
                           
        | program_sequence program_sequence
                           
*/
        ;
program_sequence:
        program end_program 
        | program  program_sequence end_program 
        | program_sequence program end_program 
/*
        | program_sequence program 
        | program program_sequence 
*/
        ;
/*end_program_opt:
        * nothing * 
        | end_program 
        ;*/
end_program:
        END PROGRAM
        
         programid_string
        
        ;
/*
 * MAIN PROGRAM STRUCTURE
 */
program:
         identification_division
         environment_division_opt
         data_division_opt
         procedure_division_opt
        ;
/*
 * IDENTIFICATION DIVISION
 */
identification_division: IDENTIFICATION DIVISION PERIOD_TOK
    PROGRAM_ID PERIOD_TOK
    
    programid_string
    
    programid_opts_opt PERIOD_TOK
    
    identification_division_options_opt
    ;
programid_string:
    IDSTRING_WORD 
    
    | IDSTRING_LITERAL 
     
    ;
programid_opts_opt:
    /*nothing */
    | is_opt INITIAL_TOK programid_program_opt 
    | is_opt COMMON programid_program_opt 
    ;
programid_program_opt:
    | PROGRAM 
    ;
identification_division_options_opt:
    /*nothing */
    | identification_division_options_opt identification_division_option
    ;
identification_division_option:
    AUTHOR PERIOD_TOK          
    | DATE_WRITTEN PERIOD_TOK  
    | DATE_COMPILED PERIOD_TOK 
    | INSTALLATION PERIOD_TOK  
    | SECURITY PERIOD_TOK      
    ;
/*
 * ENVIRONMENT DIVISION
 */
environment_division_opt:
    ENVIRONMENT DIVISION PERIOD_TOK
    
    configuration_opt
    input_output_opt
    
    | /*nothing */
    ;
/*
 * CONFIGURATION SECTION
 */
configuration_opt:
    CONFIGURATION SECTION PERIOD_TOK configuration_section
    | /*nothing */
    ;
configuration_section:
    configuration_section configuration_option  
    | /* nothing */                 
    ;
configuration_option:
    SOURCE_COMPUTER PERIOD_TOK
      
      STRING debug_mode_opt PERIOD_TOK
      
    | OBJECT_COMPUTER PERIOD_TOK
      
      STRING program_collating_opt PERIOD_TOK
      
    | SPECIAL_NAMES PERIOD_TOK special_names_opt
    | error 
    ;
program_collating_opt:
    PROGRAM collating_sequence
    | /* nothing */
    ;
collating_sequence:
    collating_opt SEQUENCE is_opt STRING
    ;
collating_opt: COLLATING  
    | /* nothing */   
    ;
debug_mode_opt:
    with_opt DEBUGGING MODE 
    | /* nothing */   
    ;
special_names_opt:
    /* nothing */
    | special_names PERIOD_TOK
    | error 
    ;
special_names:
    special_name
    | special_names special_name
    ;
special_name:
    /*special_name_opt*/
    /*symbolic_characters_opt*/
    switches_details_list
    | alphabet_details /* parsed but unimplemented */
    | currency_details
    | decimal_point_details
    | screen_status_details
    | cursor_details
    | special_name_class
/*    | error  */
/*    | DATA DIVISION  */
    ;
currency_details:
    CURRENCY sign_opt is_opt CLITERAL 
    ;
sign_opt:
    SIGN
    | /* nothing */
    ;
special_name_class:
    CLASS STRING is_opt special_name_class_item_list 
    ;
special_name_class_item_list:
    special_name_class_item 
    | special_name_class_item_list special_name_class_item 
    ;
special_name_class_item:
    CLITERAL 
    | CLITERAL THRU CLITERAL 
    ;

decimal_point_details:
    DECIMAL_POINT is_opt COMMA 
    ;
screen_status_details:
    DISPLAY_SCREEN STATUS is_opt STRING 
    
    ;
cursor_details:
    CURSOR is_opt STRING 
    
    ;
switches_details_list:
    | switches_details_list switches_details
    ;
switches_details:
    STRING is_opt STRING
    
        switch_on_opt switch_off_opt
    
    ;
switch_on_opt:
    ON status_opt is_opt STRING 
    | /* nothing */
    ;
switch_off_opt:
    OFF status_opt is_opt STRING 
    | /* nothing */
    ;
status_opt:
    STATUS
    | /* nothing */
    ;
alphabet_details:
    ALPHABET STRING is_opt alphabet_type 
    
    ;
alphabet_type:
    NATIVE 
    | STANDARD_1 
    | STANDARD_2 
    | alphabet_literal_list 
    ;
alphabet_literal_list:
    alphabet_literal_item
    | alphabet_literal_list alphabet_literal_item
    ;
alphabet_literal_item:
    without_all_literal 
    | without_all_literal THRU without_all_literal 
    | without_all_literal alphabet_also_list 
    ;
alphabet_also_list:
    ALSO without_all_literal
    | alphabet_also_list ALSO without_all_literal
    ;
/*
 * INPUT OUTPUT SECTION
 */
input_output_opt:
    INPUT_OUTPUT SECTION PERIOD_TOK input_output_section 
    | /*nothing */
    ;
input_output_section:
    file_control_paragraph i_o_control_paragraph 
    | /*nothing */
    /* | error  */
    ;
file_control_paragraph:
    FILE_CONTROL PERIOD_TOK file_control 
    ;
i_o_control_paragraph: /* nothing */ 
    | I_O_CONTROL PERIOD_TOK i_o_control_opt 
    ;

/*
 * FILE CONTROL ENTRY
 */
file_control:
    file_select
    | file_control file_select
    | /*nothing */
    ;

file_select:
    SELECT optional_opt def_name 
    
    select_clauses PERIOD_TOK 
    
    ;
select_clauses:
    select_clauses select_clause
    | /* nothing */
    ;
select_clause:
    organization_opt is_opt organization_options
                
    | ASSIGN to_opt assign_clause 
    | ACCESS mode_opt is_opt access_options 
    | file_opt STATUS is_opt strings  
    | RECORD key_opt is_opt strings   
    | RELATIVE key_opt is_opt strings 
    | ALTERNATE RECORD key_opt is_opt strings
        with_duplicates 
    | RESERVE NLITERAL areas_opt
    | error 
    ;
strings:
    STRING 
    | STRING in_of strings
    
    ;
file_opt:
    FILE_TOK
    | /* nothing */
    ;
organization_opt:
    ORGANIZATION 
    | /* nothing */
    ;
assign_clause: assign_clause_standard_files 
    | assign_clause_external_opt assign_clause_device filename 
    | assign_clause_external_opt filename 
    | assign_clause_external_opt assign_clause_device 
    ;
    
assign_clause_external_opt: 
    EXTERNAL 
    | /* nothing */ 
    ;

assign_clause_device: 
    DISK 
    | PRINTER 
    ;

assign_clause_standard_files:
    STD_INPUT  
    | STD_OUTPUT 
    | STD_ERROR  
    | DISPLAY    
    ;

with_duplicates:
    with_opt DUPLICATES 
    | /* nothing */     
    ;
optional_opt:
    OPTIONAL            
    | /* nothing */     
    ;
areas_opt: AREA 
    | AREAS 
    | /* nothing */
    ;
is_opt:
    IS 
    | /* nothing */
    ;
are_opt:
    ARE 
    | /* nothing */
    ;
mode_opt:
    MODE
    | /* nothing */
    ;
organization_options:
    INDEXED      
    | SEQUENTIAL 
    | RELATIVE   
    | LINE SEQUENTIAL 
    | anystring 
    ;
access_options:
    SEQUENTIAL  
    | DYNAMIC   
    | RANDOM    
    | anystring 
     
    ;

/* I-O CONTROL paragraph */
i_o_control_opt: /* nothing */
    | i_o_control_list PERIOD_TOK 

    ;
i_o_control_list:
    i_o_control_clause
    | i_o_control_list i_o_control_clause
    ;
i_o_control_clause:
     i_o_control_same_clause
    | i_o_control_multiple_file_tape_clause
    ;
i_o_control_same_clause:
    SAME same_clause_options are_opta for_opt string_list 
    ;
same_clause_options: /* nothing */
    | RECORD     
    | SORT       
    | SORT_MERGE 
    ;
i_o_control_multiple_file_tape_clause:
    MULTIPLE FILE_TOK tape_opt contains_opt i_o_control_multiple_file_list
    ;
i_o_control_multiple_file_list:
    i_o_control_multiple_file
    | i_o_control_multiple_file_list i_o_control_multiple_file
    ;
i_o_control_multiple_file:
    STRING i_o_control_multiple_file_position_opt 
    ;
i_o_control_multiple_file_position_opt:
    | POSITION integer 
    ;
tape_opt: 
    TAPE
    | /* nothing */
    ;
are_opta: 
    AREA 
    | /* nothing */
    ;
for_opt: 
    FOR 
    | /* nothing */
    ;
string_list:
    STRING 
    | string_list STRING 
    | error 
    ;
name_list:
    variable 
    | name_list variable 
    | error 
    ;
/* end enviroment division */

/*
 * DATA DIVISION
 */
data_division_opt: 
    DATA DIVISION PERIOD_TOK 
    
    file_section_opt
    working_storage_opt
    linkage_section_opt
    report_section_opt
    screen_section_opt       
    
    | /* nothing */
;
/*
 *  FILE SECTION
 */
file_section_opt:
    FILE_TOK SECTION PERIOD_TOK  
    file_section   
    | /* nothing */
    ;
/*
 *  WORKING STORAGE SECTION
 */
working_storage_opt:
    WORKING_STORAGE SECTION PERIOD_TOK 
    working_storage_section 
    | /* nothing */
    ;
/*
 * LINKAGE SECTION
 */
linkage_section_opt:
    LINKAGE SECTION PERIOD_TOK 
    linkage_section            
    | /* nothing */
    ;

/*
 * COMMUNICATION SECTION
 */

/* Not Implemented */

/*
 * REPORT SECTION
 */
/* Work in progress - not finished */
report_section_opt:
    REPORT SECTION PERIOD_TOK
    report_sections 
    | /* nothing */
    ;
report_sections: 
    report_sections report_section 
    | /* nothing */
    ;
report_section:
    RD 
    
    STRING 
    
    report_controls PERIOD_TOK
    
    report_description
     
    ;
report_controls:   /* nothing  */ 
    | report_controls report_control
    ;
report_control:  is_opt GLOBAL  
    | CODE gliteral  
    | report_controls_control  
    | report_controls_page 
    ;
report_controls_control:
    control_is_are final_opt report_break_list
    ;
report_controls_page:
    PAGE_TOK limit_is_are_opt integer line_lines_opt
      heading_opt
      first_detail_opt last_detail_opt
      footing_opt
    ;
heading_opt:   /* nothing */ 
    | HEADING is_opt integer
    ;
line_lines_opt:
    lines_opt
    | LINE
    ;
lines_opt:
    /* nothing */
    | LINES
    ;
control_is_are:
    CONTROL is_opt
    | CONTROLS are_opt
    ;
limit_is_are_opt:
    /* nothing */
    | LIMIT IS
    | LIMITS ARE
    ;
footing_opt:
    /* nothing */
    | FOOTING is_opt integer
    ;
last_detail_opt:
    /* nothing */
    | LAST DETAIL is_opt integer
    ;
first_detail_opt:
    /* nothing */
    | FIRST DETAIL is_opt integer
    ;
final_opt:
    /* nothing */
    | FINAL
    ;
report_break_list:
    /* nothing */
    | report_break_list name 
    ;
report_description:
    report_item
    | report_description report_item
    ;
report_item:
    integer def_name_opt 
    
    report_clauses PERIOD_TOK
    
    ;
report_clauses:
     report_clause 
    | report_clauses report_clause
    ;
report_clause:
    report_clause_line 
    | report_clause_next_group 
    | report_clause_type 
    | report_clause_usage_display 
    | report_clause_picture 
    | report_clause_sign_is 
    | report_clause_justified 
    | report_clause_blank_zero 
    | report_clause_column 
    | report_clause_svs 
    | report_clause_group_indicate 
    ;
report_clause_type:
    TYPE is_opt report_clause_type2 
    ;
report_clause_type2:
    REPORT HEADING 
    | PAGE_TOK HEADING 
    | CONTROL HEADING  
      name_final_opt  
    | DETAIL 
    | CONTROL FOOTING  
      name_final_opt 
    | PAGE_TOK FOOTING 
    | REPORT FOOTING 
    | RH 
    | PH 
    | CH   
      name_final_opt 
    | DE 
    | CF   
      name_final_opt 
    | PF 
    | RF 
    ;
report_clause_sign_is:
    SIGN is_opt leading_trailing SEPARATE character_opt 
    | leading_trailing SEPARATE character_opt 
    ;
report_clause_picture:
    PICTURE 
    
    is_opt picture 
    ;
report_clause_usage_display:
    USAGE is_opt DISPLAY 
    | DISPLAY 
    ;
report_clause_justified:
    JUSTIFIED right_opt
    ;
report_clause_next_group:
    NEXT GROUP is_opt integer_on_next_page
    ;
report_clause_line:
    report_clause_line_is integer ON NEXT PAGE_TOK 
    | report_clause_line_is integer NEXT PAGE_TOK 
    | report_clause_line_is integer 
    | report_clause_line_is PLUS integer 
    ;
report_clause_line_is:
    LINE is_are_opt
    | LINE NUMBER is_opt
    | LINE NUMBERS are_opt
    | LINES are_opt
    ;
report_clause_column: 
    report_clause_column_is integer 
    ;
report_clause_column_is:
    COLUMN report_clause_column_orientation is_are_opt        
    | COLUMN NUMBER report_clause_column_orientation is_opt   
    | COLUMN NUMBERS report_clause_column_orientation are_opt 
    | COLUMNS report_clause_column_orientation are_opt        
    ;
report_clause_column_orientation:
    /* nothing */ 
    | LEFT   
    | CENTER 
    | RIGHT  
    ;
is_are_opt:
    /* nothing */
    | IS 
    | ARE 
    ;
report_clause_svs: SOURCE 
      
      is_opt gname_page_counter 
      
    | VALUE is_opt literal 
    | SUM 
      gname_list 
      upon_opt reset_opt 
    ;
gname_page_counter: gname 
    | PAGE_COUNTER 
    ;
report_clause_group_indicate:
    | GROUP indicate_opt 
    ;
report_clause_blank_zero:
    BLANK when_opt ZERO 
    ;
indicate_opt:
    | INDICATE 
    ;
upon_opt:
    | UPON gname_list 
    ;
reset_opt:
    | RESET gname 
    | RESET FINAL 
    ;
number_opt:
    | NUMBER 
    ;
leading_trailing:
    LEADING    
    | TRAILING 
    ;
right_opt:
    | RIGHT 
    ;
name_final_opt:
    gname 
    | FINAL 
    ;
integer_on_next_page:
    integer         
    | PLUS integer  
    | NEXT PAGE_TOK 
    ;             
of_opt:
    OF
    | /* nothing */
    ;
/* end report section */

/*
 *  SCREEN SECTION
 */

screen_section_opt:
    SCREEN SECTION PERIOD_TOK  
    
    screen_section  
    | 
    /* nothing
    
    */
    ;

/*
 *  SCREEN SECTION DESCRIPTION ENTRY
 */
screen_section:
    screen_section screen_item
    | /* nothing */
    ;
screen_item:
    integer def_name_opt    
    
    screen_clauses PERIOD_TOK  
    ;
screen_clauses:
    screen_clauses LINE          
        number_is_opt
        plus_minus_opt
        name_or_lit              
    | screen_clauses COLUMN     
        number_is_opt
        plus_minus_opt
        name_or_lit             
    | screen_clauses
        with_opt screen_attrib          
    | screen_clauses with_opt FOREGROUNDCOLOR    
        name_or_lit                     
    | screen_clauses with_opt BACKGROUNDCOLOR    
        name_or_lit                     
    | screen_clauses with_opt COLOR              
        name_or_lit                     
    | screen_clauses
        screen_source_destination
    | screen_clauses
        value_is_are gliteral 
    | screen_clauses pictures
    | screen_clauses SIZE     
                name_or_lit   
    | /* nothing */         
    ;
screen_source_destination:
    USING                    
                name_or_lit     
    | FROM                    
                name_or_lit
                screen_to_name  
    | TO 
                name             
    ;
screen_to_name:
    /* nothing */ 
    | TO name 
    ;
screen_attrib:
    BLANK SCREEN                    
    | BLANK LINE                    
    | ERASE EOL                     
    | ERASE EOS                     
    | ERASE                            
    | with_opt BELL                    
    | sign_clause                    
    | FULL                            
    | REQUIRED                      
    | SECURE                            
    | AUTO                            
    | JUSTIFIED RIGHT                    
    | JUSTIFIED LEFT                    
    | BLINK                            
    | REVERSEVIDEO                    
    | UNDERLINE                     
    | LOWLIGHT                      
    | HIGHLIGHT                     
    | BLANK when_opt ZERO            
    | with_opt NOECHO                    
    | with_opt UPDATE                    
    | with_opt NO ADVANCING            
    | UPPER                            
    | LOWER                            
    ;
sign_clause:
    sign_is_opt LEADING separate_opt
            
    | sign_is_opt TRAILING separate_opt
            
    ;
separate_opt:
    SEPARATE character_opt  
    | /* nothing */         
    ;
character_opt:
    CHARACTER
    | /* nothing */
    ;
sign_is_opt:
    SIGN is_opt
    | is_opt
    ;
plus_minus_opt:
    PLUS            
    | '+'           
    | MINUS         
    | '-'           
    | /* nothing */ 
    ;
number_is_opt:
    NUMBER is_opt
    | /* nothing */
    ;
/* end screen section */

/*
 * File description entry
*/
file_section:
    file_section FD 
     
     STRING 
     
     file_description_fd_clauses PERIOD_TOK
     
     file_description        
     
    | file_section SD 
      STRING 
      file_description_sd_clauses PERIOD_TOK
      
      file_description    
      
    | error 
    | /* nothing */
    ;
file_description:
    field_description 
    
    | file_description field_description 
    
    ;

/*
 * DATA DESCRIPTION ENTRY
 */
field_description:
    integer def_name_opt 
     
    data_clauses PERIOD_TOK 
    
    ;
data_clauses:
    /* nothing */
    | data_clauses data_clause
    | data_clauses redefines_clause
    ;
redefines_clause:
    REDEFINES
    
    redefines_var
    
    ;
redefines_var:
    VARIABLE    
    | SUBSCVAR  
    ;
data_clause:
    array_options
    | pictures
    | usage_option
    | sign_clause 
    | value_option
    | SYNCHRONIZED sync_options 
    | JUSTIFIED sync_options 
    | is_opt EXTERNAL 
    | is_opt GLOBAL 
    | BLANK when_opt ZERO 
    | RENAMES 
                variable thru_gname_opt
                    
    ;
sync_options:
    /* nothing */ 
    | LEFT        
    | RIGHT        
    ;
thru_gname_opt: /* nothing */ 
    | THRU variable 
    ;

/* OCCURS clause */
array_options:
      OCCURS integer times_opt
      
      indexed_by_opt
    | OCCURS integer TO integer times_opt DEPENDING
      
      on_opt gname
      
      indexed_by_opt
    ;
key_is_opt:
    DIRECTION key_opt is_opt STRING
    
    | 
    ;
indexed_by_opt: 
    key_is_opt INDEXED by_opt index_name_list
    
    | /* nothing */
    ;
index_name_list:
     def_name        
        
     | index_name_list def_name 
         
     ;

/* USAGE clause */

usage_option :
    usage_opt is_opt usage 
    ;
usage:
    USAGENUM    
    | DISPLAY   
    | POINTER   
    ;

/* VALUE clause */

value_option:
    value_is_are value_list
    ;
value_is_are:
    VALUE is_opt
    | VALUES are_opt
    ;
value_list:
        value                           
        | value_list comma_opt value
        ;
value:
    gliteral    
    | gliteral THRU gliteral
     
    ;

/* Pictures clause */

pictures:
    PICTURE 
    
    is_opt picture 
    
    ;
picture: /* nothing */
    | picture pic_elem 
    ;
pic_elem:
    CHAR multiplier_opt 
            
    ;
multiplier_opt:
    /* nothing */ 
    | MULTIPLIER 
    ;

/* File description entry  */
file_description_fd_clauses: 
    | file_description_fd_clauses file_description_fd_clause
     
    ;
file_description_sd_clauses: 
    | file_description_sd_clauses file_description_sd_clause 
     
    ;

file_description_fd_clause:
    is_opt EXTERNAL                    
    | is_opt GLOBAL                    
    | file_description_clause_block    
    | file_description_clause_record   
    | file_description_clause_label    
    | file_description_clause_value    
    | file_description_clause_data     
    | file_description_clause_report    
    | file_description_clause_linage   
    | file_description_clause_code_set 
    ;
file_description_sd_clause:
    file_description_clause_record   
    | file_description_clause_data   
    ;

file_description_clause_block:
    BLOCK contains_opt integer
    to_integer_opt chars_or_recs_opt 
    
    ;

file_description_clause_record:
    RECORD contains_opt nliteral
    to_rec_varying_opt character_opts 
    
    | RECORD is_opt VARYING in_opt size_opt
      from_rec_varying_opt to_rec_varying_opt character_opts
      depend_rec_varying_opt
      
    ;

file_description_clause_label: 
    LABEL record_is_are std_or_omitt 
    ;

file_description_clause_value: 
    VALUE OF FILE_ID is_opt filename 
    
    ;

file_description_clause_data: 
    DATA record_is_are var_strings   
    ;

file_description_clause_report: 
    report_is_are STRING 
    ;

file_description_clause_code_set:
    CODE_SET is_opt STRING 
    
    ;

file_description_clause_linage:
    LINAGE is_opt data_name lines_opt
    file_description_clause_linage_footing 
    file_description_clause_linage_top 
    file_description_clause_linage_bottom
    
    ;

file_description_clause_linage_footing:
    | with_opt FOOTING at_opt data_name 
    ;

file_description_clause_linage_top:
    | lines_at_opt TOP data_name 
    ;

file_description_clause_linage_bottom:
    | lines_at_opt BOTTOM data_name 
    ;

lines_at_opt:  /* nothing */
    | LINES
    | LINES AT
    | AT
    ;

report_is_are: REPORT is_opt
    | REPORTS are_opt
    ;

var_strings: STRING 
    | var_strings STRING 
    ; 
chars_or_recs_opt:
    /* nothing */
    | CHARACTERS
    | RECORDS
    ;
to_integer_opt: /* nothing */
    | TO integer 
    ;
depend_rec_varying_opt: 
    | DEPENDING on_opt STRING 
    ;
from_rec_varying_opt:
    /* nothing */ 
    | from_opt nliteral 
    ;
from_opt: 
    FROM 
    | /* nothing */
    ;             
to_rec_varying_opt:
    /* nothing */ 
    | TO nliteral 
    ;
record_is_are:
    RECORD is_opt
    | RECORDS are_opt
    ;
std_or_omitt:
    STANDARD
    | OMITTED
    ;
usage_opt:
    /* nothing */
    | USAGE
    ;
times_opt:
    /* nothing */
    | TIMES
    ;
when_opt:
    /* nothing */
    | WHEN
    ;
contains_opt:
    /* nothing */
    | CONTAINS
    ;
character_opts:
    /* nothing */
    | CHARACTERS
    ;
order_opt:
    /* nothing */
    | ORDER
    ;
data_opt:
    /* nothing */
    | DATA
    ;
/*
 * DATA DIVISION - working storage section
 */    
working_storage_section:
    working_storage_section
     field_description
    | /* nothing */
    ;
/*
 * DATA DIVISION - linkage section
 */    
linkage_section:
    /* nothing */
    | linkage_section
        field_description
    ;

/* PROCEDURE DIVISION */
procedure_division_opt:
    PROCEDURE DIVISION
    
     procedure_division_using_chaining_opt 
     procedure_division_returning_opt 
     PERIOD_TOK 
    
     declaratives_opt
     procedure_list
    
    | /* nothing */
    ;

/* Procedure Division Declaratives */
declaratives_opt: 
     DECLARATIVES PERIOD_TOK
     
     declaratives_procedure
     declaratives_procedures
     END DECLARATIVES PERIOD_TOK
     
     | /* nothing */
    ;

declaratives_procedures: declaratives_decl  
    | declaratives_procedures declaratives_decl 
    ;

declaratives_procedure:
    procedure_section
    
    use_statements 
    
    | error 
    ;

declaratives_decl: declaratives_procedure
    | paragraph 
    |  statements PERIOD_TOK
    | error  PERIOD_TOK
    ;

use_statements:
    use_exception   
    | use_debugging 
    | use_reporting 
    ;

use_exception: 
    USE use_global_opt AFTER use_exception_error 
    PROCEDURE on_opt use_exception_option PERIOD_TOK
    
    | error 
    ;
use_exception_error: EXCEPTION 
    | ERROR_TOK 
    | STANDARD EXCEPTION 
    | STANDARD ERROR_TOK 
    | error 
     
    ;
use_exception_option:
    /* name  */
    use_exception_option_names 
    | open_mode
     
    ;

use_exception_option_names:
    name 
    
    | use_exception_option_names comma_opt name 
     
    ;

use_reporting: USE use_global_opt BEFORE REPORTING name 
    
    ;

use_debugging: USE for_opt DEBUGGING on_opt
    use_debugging_options
    
    ;
use_debugging_options: use_debugging_options_procedure_names
    /* gname_list // procedure-name-1 ... */
    /* name // procedure-name-1 */
    | ALL PROCEDURES
    ;
use_debugging_options_procedure_names: name
    | use_debugging_options_procedure_names name
    ;

use_global_opt: 
    | GLOBAL 
    ;

procedure_list:
    | procedure_list procedure_decl
    ;
procedure_decl:
    procedure_section 
    | paragraph 
    |  statements PERIOD_TOK
/*    |  statements */
    | error  PERIOD_TOK
    | PERIOD_TOK 
    ;

/* Procedure division sections and paragraphs */

procedure_section:
    LABELSTR SECTION PERIOD_TOK
    
    ;
paragraph:
    LABELSTR dot_or_eos
    
    ;
dot_or_eos:
    '.'
    | PERIOD_TOK
    ;

/*
 * PROCEDURE DIVISION - COBOL verbs
 */

statement_list:
    statements 
    ;
statements:
    statement
    | statements statement
    ;
statement:
      move_statement
    | initialize_statement
    | compute_statement
    | add_statement
    | subtract_statement
    | multiply_statement
    | divide_statement
    | accept_statement
    | display_statement
    | open_statement
    | close_statement
    | read_statement
    | return_statement
    | release_statement
    | write_statement
    | rewrite_statement
    | delete_statement
    | start_statement
    | perform_statement
    | goto_statement
    | exit_statement
    | stop_statement
    | call_statement
    | call_loadlib_statement
    | chain_statement
    | set_statement
    | sort_statement
    | merge_statement
    | inspect_statement
    | string_statement
    | unstring_statement
    | initiate_statement
    | generate_statement
    | terminate_statement
    | proto_statement
    | trace_statement
    | goback_statement
    | cancel_statement
    | unlock_statement
    | if_statement
    | evaluate_statement
    | search_statement
    | CONTINUE 
/*
    | NEXT SENTENCE 
*/
    ;

perform_statement:
    PERFORM perform_options
    ;
if_statement:
    if_part  end_if_opt
    | if_part ELSE
    
     /* sentence  */
    conditional_statement 
    end_if_opt
    ;
search_statement:
    SEARCH search end_search_opt
    | SEARCH ALL search_all end_search_opt
    ;
evaluate_statement:
    EVALUATE
    
    selection_subject_set
    
    when_case_list
    end_evaluate_or_eos
    
    ;
end_evaluate_or_eos:
    END_EVALUATE
    | PERIOD_TOK
    ;
selection_subject_set:
    
    selection_subject 
    | selection_subject_set ALSO
     
     selection_subject
     
    ;
selection_subject:
    expr    /* this already includes identifiers and literals */
    
    | condition 
    | TRUE_TOK  
    | FALSE_TOK 
    ;
when_case_list:
    WHEN 
    
    when_case
    sentence_or_nothing
    
    | when_case_list WHEN 
      
      when_case
      
      sentence_or_nothing
      
    ;
when_case:
    
    selection_object 
    
    | when_case ALSO
      
      selection_object 
      
    | OTHER 
    ;
selection_object:
    ANY         
    | TRUE_TOK  
    | FALSE_TOK 
    | not_opt expr
     
    | not_opt expr THRU expr
      
    | not_opt cond_name 
     
    ;

sentence_or_nothing:
    /* nothing */               
    | conditional_statement     
    ;

if_part:
    IF  condition  
    
    end_then_opt
    conditional_statement 
    /* sentence   */
    ;

/*
  Fix me: This does not conform to the ANSI85 standard.
  However, it does reduce the number of conflicts.
*/
conditional_statement:  statement_list
    |   NEXT SENTENCE
/*
    |   CONTINUE 
*/
    ;

not_opt:
    /* nothing */ 
    | NOT 
    ;
end_if_opt:
    | END_IF
    ;
end_then_opt:
    | THEN
    ;
search:
    variable_indexed
    
    search_varying_opt
    
    search_at_end
     
    search_when_list
    
    ;
search_all:
     variable_indexed
     
     search_at_end
     
     search_all_when_list
     
    ;

search_varying_opt:
    VARYING variable 
    | 
    ;

search_at_end:
     at_opt END
     
     statement_list
     
    |
     
    ;

search_when_list:
     search_when 
     | search_when_list search_when 
     ;
search_when:
     WHEN
     search_when_conditional
     
     search_when_statement
     
     ;

search_when_statement:
    statement_list   
    |  NEXT SENTENCE
/*
    |  CONTINUE 
*/
    ;

search_when_conditional:
/*    name cond_op name 
    | name cond_op nliteral 
    | name extended_cond_op name_or_lit 
    | name_or_lit extended_cond_op name 
    | nliteral cond_op nliteral  */
    name_or_lit extended_cond_op name_or_lit 
    ;

search_all_when_list:
     search_all_when
     | search_all_when_list search_all_when
     ;
search_all_when:
     WHEN 
     search_all_when_conditional
     
     search_all_when_statement
     
    ;

search_all_when_statement:
    statement_list   
/*    statement */
    |  NEXT SENTENCE
/*
    |  CONTINUE 
*/
    ;

search_all_when_conditional:
    variable is_opt CONDITIONAL to_opt variable 
    
    | variable is_opt CONDITIONAL to_opt literal
      
    | search_all_when_conditional AND 
      search_all_when_conditional  
    ;

end_search_opt:
    | END_SEARCH
    ;

unlock_statement:
    UNLOCK name 
    
    ;
proto_statement:
    TCOBPROTO1 gname 
    | TCOBPROTO2 gname gname 
    ;
trace_statement:
    READY TRACE 
    | RESET TRACE 
    ;
initiate_statement:
    INITIATE name
    ;
generate_statement:
    GENERATE name
    ;
terminate_statement:
    TERMINATE name
    ;
cancel_statement:
    CANCEL gname 
    | CANCEL ALL 
    ;
        
/* MERGE statement */
merge_statement:
    MERGE name sort_keys 
    sort_collating_opt 
    merge_using 
    sort_output 
    
    ;

merge_using:
    USING sort_file_list 
    
    ;

/* SORT statement */
sort_statement:
    SORT name sort_keys 
    sort_duplicates_opt sort_collating_opt 
    sort_input
    sort_output 
    
    ;

sort_keys:
    sort_key 
    | sort_keys sort_key 
    ;

sort_key:
    on_opt DIRECTION key_opt sort_keys_names
    
    ;

sort_keys_names:
    name 
    | sort_keys_names name 
    ;

sort_duplicates_opt:
    | with_opt DUPLICATES in_opt order_opt 
      
    ;

sort_collating_opt:
    | collating_sequence 
     
    ;

/* SORT and MERGE statement clauses */  
sort_input:
    INPUT PROCEDURE is_opt perform_range 
     
    | USING sort_file_list 
     
    ;       
sort_output:
    OUTPUT PROCEDURE is_opt perform_range 
     
    | GIVING sort_file_list 
     
    ;

sort_file_list:  name 
     
    | sort_file_list name
     
    ;
        
/* MOVE statement */
move_statement:
      MOVE gname TO name_var_list 
    | MOVE CORRESPONDING name_var TO name_var 
    | MOVE LENGTH OF gname TO name_var 
    | MOVE gname TO 
    ;
/* INITIALIZE statement */
initialize_statement:
    INITIALIZE  gname_list initialize_replacing_opt 
    | INITIALIZE 
    ;
initialize_replacing_opt:
    | REPLACING initialize_replacing_lists 
     
    ;
initialize_replacing_lists:
    initialize_replacing_list
    | initialize_replacing_lists initialize_replacing_list 
    ;
initialize_replacing_list:
    initialize_type_list data_opt BY gname 
    ;
initialize_type_list: ALPHABETIC_TOK 
    | ALPHANUMERIC 
    | NUMERIC 
    | ALPHANUMERIC_EDITED 
    | NUMERIC_EDITED 
    ;
     
/* Compute statement */
compute_statement:
    COMPUTE compute_body end_compute_opt
    | COMPUTE 
    ;
compute_body:
    var_list_name CONDITIONAL expr on_size_error_opt
    
    ;
end_compute_opt:
    /* nothing */
    | END_COMPUTE
    ;

/* Add statement */
add_statement:
    ADD add_body end_add_opt
    | ADD   
    ;
add_body:
    var_list_gname TO var_list_name on_size_error_opt
    
    | var_list_gname add_to_opt GIVING var_list_name on_size_error_opt
      
    | CORRESPONDING var_list_gname TO var_list_name rounded_opt on_size_error_opt
      
    ;
add_to_opt:
    /* nothing */ 
    | TO gname    
    ;
end_add_opt:
    /* nothing */
    | END_ADD
    ;

/* Subtract statement */

subtract_statement:
    SUBTRACT subtract_body end_subtract_opt
    | SUBTRACT 
    ;
subtract_body:
    var_list_gname FROM var_list_name on_size_error_opt
      
    | var_list_gname FROM gname GIVING var_list_name on_size_error_opt
      
    | CORRESPONDING var_list_gname FROM var_list_name rounded_opt on_size_error_opt
      
    ;
end_subtract_opt:
    | END_SUBTRACT
    ;

/* Multiply statement */

multiply_statement:
    MULTIPLY multiply_body end_multiply_opt
    | MULTIPLY 
    ;
multiply_body:
    gname BY gname GIVING var_list_name on_size_error_opt
      
    | gname BY var_list_name on_size_error_opt
      
    ;
end_multiply_opt:
    /* nothing */
    | END_MULTIPLY
    ;

/* Divide statement */

divide_statement:
    DIVIDE divide_body end_divide_opt
    | DIVIDE 
    ;
divide_body:
    gname BY gname GIVING var_list_name on_size_error_opt
      
    | gname BY gname GIVING name rounded_opt REMAINDER name on_size_error_opt
      
    | gname INTO gname GIVING name rounded_opt REMAINDER name on_size_error_opt
      
    | gname INTO gname GIVING var_list_name on_size_error_opt
      
    | gname INTO var_list_name on_size_error_opt
      
    ;
end_divide_opt:
    /* nothing */
    | END_DIVIDE
    ;

/* Accept statement */
accept_statement:
    accept_hardware            /* Accept format 1 (hardware)      */ 
    | accept_chronological     /* Accept format 2 (chronological) */
    | accept_screen            /* Accept format 3 (screen)        */
    | ACCEPT 
    ;
accept_hardware:
      ACCEPT name accept_from_opt  on_exception_opt end_accept_opt
    | ACCEPT name FROM INKEY        end_accept_opt 
    | ACCEPT name FROM INPUT STATUS end_accept_opt 
    | ACCEPT name FROM CMD_LINE     end_accept_opt  
    | ACCEPT name FROM ENVIRONMENT_VARIABLE accept_hardware_env_var end_accept_opt  
    
    ;
accept_hardware_env_var: name 
    | CLITERAL   
     
    ;
accept_from_opt:
    | FROM STD_INPUT
    | FROM CONSOLE
    ;
on_exception_opt:
    | on_opt EXCEPTION variable 
    
      statement_list    
    ;
accept_chronological:
    ACCEPT name FROM DATE_TIME end_accept_opt       
    
    ;
accept_screen:
    ACCEPT name accept_options end_accept_opt
                         
    | ACCEPT name accept_options 
      on_opt EXCEPTION   
      variable          
      statement_list    
      end_accept_opt         
    ;
end_accept_opt:
     /* Nothing */
     | END_ACCEPT 
     ;

/* Display statement */
display_statement:
    display_line 
    | display_screen 
    | DISPLAY 
    ;
display_line:
    DISPLAY display_varlist display_upon display_line_options end_display_opt
      
    ;
display_screen:
    DISPLAY display_varlist accept_display_options end_display_opt
    
    ;
display_varlist:
    gname                           
    | display_varlist sep_opt gname 
    ;
display_upon:
    UPON CONSOLE       
    | UPON STD_OUTPUT  
    | UPON STD_ERROR   
    ;
display_line_options:
     /* nothing */                                
    | display_line_options with_opt NO ADVANCING 
    | display_line_options with_opt ERASE        
    | display_line_options with_opt ERASE EOS    
    | display_line_options with_opt ERASE EOL    
    | display_line_options with_opt ERASE SCREEN 
    ;
end_display_opt:
     /* Nothing */
     | END_DISPLAY
     ;
/* common options for display and accept */
/*line_position:    
    | at_opt scr_line  scr_position  
    | scr_line_position        
    ;*/
scr_line:    
    LINE number_opt expr
     
    ;
scr_position:    
     COLUMN number_opt expr 
     
    | POSITION expr 
     
    ;
scr_line_position:
    AT NLITERAL
      
    | AT variable
      
    ;
accept_options:
    accept_display_option 
    | accept_options accept_display_option 
    ;
accept_display_options:
    /* nothing */                   
    | accept_display_options accept_display_option 
    ;
accept_display_option:
    with_opt  screen_attrib     
    | scr_line                  
    | scr_position              
    | scr_line_position         
    ;

/* Open statement */
open_statement:
    OPEN open_options 
    
    | OPEN 
    ;
open_options:
    open_mode open_varlist 
    | open_options open_mode open_varlist 
    ;
open_mode:
    INPUT    
    | I_O    
    | OUTPUT 
    | EXTEND 
    | error  
     
    ;
open_varlist:
      name 
    | open_varlist sep_opt name 
    ;

/* Close statement */
close_statement:
    CLOSE close_files
    | CLOSE 
    ;
close_files:
      close_file
    | close_files sep_opt close_file
    ;
close_file:
    name close_options_opt 
    ;
close_options_opt:
    close_options   
    
    | with_lock_opt 
    ;
close_options:
    with_opt NO REWIND 
    | REEL         
    | UNIT         
    | REEL for_opt REMOVAL 
    | UNIT for_opt REMOVAL 
    ;
with_lock_opt:
    with_opt LOCK          
    
    | with_opt IGNORE LOCK 
     
    | /* nothing */        
    ;

/* Return statements */

return_statement: 
    RETURN_TOK return_body end_return_opt
    | RETURN_TOK 
    ;
return_body:
    name
    record_opt
    read_into_opt
    
    | name
    record_opt
    read_into_opt
    read_at_end_opt
    
    ;

/* Read statements */

read_statement: 
    READ read_body end_read_opt 
    | READ 
    ;
read_body: 
    name
    read_next_opt
    record_opt
    read_into_opt
    with_lock_opt
    read_key_opt
    
    | name
    read_next_opt
    record_opt
    read_into_opt
    with_lock_opt
    read_key_opt
    read_at_end_opt
    
   | name
    read_next_opt
    record_opt 
    read_into_opt
    with_lock_opt
    read_key_opt
    read_invalid_key_opt 
    
    ;
read_next_opt:
    /* nothing */       
    | NEXT                
    | PREVIOUS            
    ;
read_into_opt:
    /* nothing */       
    | INTO name         
    ;
read_key_opt:
    /* nothing */       
    | KEY is_opt name   
    ;
read_at_end_opt:
    NOT at_opt on_end       
     
    | AT on_end 
     
    | on_end
     
    | AT on_end NOT at_opt 
      
     on_end 
     
    | on_end NOT at_opt 
      
     on_end 
     
    ;
on_end:
    END
    
    statement_list
    
    ;
read_invalid_key_opt:
    read_invalid_key 
    | read_not_invalid_key 
    | read_invalid_key read_not_invalid_key 
    ;
read_invalid_key:
    INVALID key_opt     
    statement_list            
    ;
read_not_invalid_key:
    NOT INVALID key_opt 
    statement_list            
    ;
end_read_opt:
    /* nothing */
    | END_READ
    ;
end_return_opt:
    /* nothing */
    | END_RETURN
    ;

/* Release statement */

release_statement:
    RELEASE name release_from_opt
    
    ;
release_from_opt:
    /* nothing */       
    | FROM gname        
    ;

/* Write statement */

write_statement:
    WRITE name write_from_opt write_options
     /*
      if ($2->level != 1) 
      else 
    }*/
    invalid_key_opt
    end_write_opt 
    
    ;
write_from_opt:
    /* nothing */       
    | FROM gname        
    ;
write_options:
    /* nothing */       
    | before_after advancing_opt gname line_lines_opt
      
    | before_after advancing_opt PAGE_TOK
      
    ;
end_write_opt:
    /* nothing */
    | END_WRITE
    ;

/* Rewrite statement */

rewrite_statement:
    REWRITE name write_from_opt
     /*
     if ($2->level != 1)
        yyerror("Identifier %s may not be used in REWRITE statement", $2->name);
     gen_rewrite($2, $3);
    } */
    invalid_key_opt
    end_rewrite_opt 
    
    ;
end_rewrite_opt:
    /* nothing */
    | END_REWRITE
    ;

/* Delete statement */

delete_statement:
    DELETE name record_opt 
    
    invalid_key_opt
    end_delete_opt 
    
    ;
end_delete_opt:
    /* nothing */
    | END_DELETE
    ;

/* Start statement */

start_statement:
    START start_body
    invalid_key_opt 
    
    end_start_opt
    ;
start_body:
    name 
    
    | name KEY is_opt cond_op name 
    
    ;
end_start_opt:
    /* nothing */
    | END_START
    ;

/* 
 GO TO statements 
 Format 1: Unconditional
 Format 2: Conditional  (DEPENDING ON)
 Format 3: Altered (Paragraph-name. GO TO.) - not implemented 
*/
goto_statement:
    GO to_opt goto_label 
    | GO to_opt goto_label_list DEPENDING on_opt variable
    
    ;
goto_label: label 
    ;
goto_label_list:
      label                             
    | goto_label_list label             
    | goto_label_list LISTSEP label     
    ;

/* CALL statement */
call_statement:
    CALL  
    call_convention_opt
    gname
    using_options       
    returning_options 
    
     
    
    on_exception_or_overflow 
    on_not_exception 
    end_call_opt
    | CALL 
    ;

call_convention_opt: 
    | CALL_CONV_C 
    | CALL_CONV_STDCALL 
/*    | CALL_CONV_STDCALL */
    ;

/* CALL-LOADLIB statement */
call_loadlib_statement:
    CALL_LOADLIB
    gname
    
    end_call_loadlib_opt
    | CALL_LOADLIB 
    ;

/* END-CALL-LOADLIB  */
end_call_loadlib_opt: END_CALL_LOADLIB 
    | 
    ;

/* CHAIN statement */        
chain_statement:        
    CHAIN  
    gname
    using_options   
    
    on_exception_or_overflow 
    
    end_chain_opt
    | CHAIN 
    ;

/* EXIT statement */
exit_statement:
      EXIT           
    | EXIT PARAGRAPH 
    | EXIT PROGRAM   
    ;

/* Stop statement */
stop_statement:
    STOP RUN 
    | STOP stop_literal
     
    ;
stop_literal:
    CLITERAL
    
    | NLITERAL
    
    ;

/* Goback statement */
goback_statement:
    GOBACK 
    ;

var_list_name: name rounded_opt sep_opt
     
    | var_list_name name rounded_opt sep_opt
     
    ;

var_list_gname: gname sep_opt
     
    | var_list_gname gname sep_opt
     
    ;

rounded_opt:
    /* Nothing */  
    | ROUNDED      
    ;

on_size_error_opt:
    /* nothing */
      
    | NOT on_opt SIZE
      on_size_error
      
    | on_opt SIZE
      on_size_error
      
    | on_opt SIZE
      on_size_error
      NOT on_opt SIZE
      
      on_size_error
      
    ;

on_size_error:
     ERROR_TOK
     
     statement_list
     
    ;

size_opt:
    /* nothing */
    | SIZE
    ;
end_call_opt:
    /* nothing */
    | END_CALL
    ;
end_chain_opt:
    /* nothing */
    | END_CHAIN
    ;
    
/* SET statement */  
set_statement:
    SET set_list ;
set_list: 
    set_target TO address_of_opt set_variable_or_nlit
       
  | set_target UP BY var_or_nliteral   
       
  | set_target DOWN BY var_or_nliteral 
       
  | address_of_opt variable TO address_of_opt set_variable 
       
  ;

set_target:
/*    variable   */
    name_list  
  | cond_name 
  ;

set_variable:
   variable           
  | NULL_TOK           
  ;

set_variable_or_nlit:
  name_or_lit          
  | ON                  
  | OFF                  
  | NULL_TOK          
  | TRUE_TOK          
   
  ;

address_of_opt:
  /* nothing */ 
  | ADDRESS of_opt 
  ;
        
/* String and Unstring statements */

string_statement:
    STRINGCMD string_from_list
        INTO name string_with_pointer 
        on_overflow_opt
        end_stringcmd_opt
    ;
unstring_statement:
    UNSTRING name
        unstring_delimited
        INTO unstring_destinations
        string_with_pointer
        unstring_tallying 
        on_overflow_opt
        end_unstring_opt
    ;
unstring_delimited:
    DELIMITED by_opt unstring_delimited_vars 
    | /* nothing */                          
    ;
unstring_delimited_vars:
    all_opt gname       
    | unstring_delimited_vars OR all_opt gname 
    ;
unstring_destinations:
    unstring_dest_var       
    | unstring_destinations sep_opt
        unstring_dest_var   
    ;
unstring_dest_var:
    name unstring_delim_opt unstring_count_opt 
    ;
unstring_delim_opt:
    /* nothing */           
    | DELIMITER in_opt name 
    ;
unstring_count_opt:
    /* nothing */           
    | COUNT in_opt name   
    ;
unstring_tallying:
    /* nothing */           
    | TALLYING in_opt name  
    ;
all_opt:
    /* nothing */           
    | ALL                   
    ;
on_overflow_opt:
    
    on_overflow
    on_not_overflow
    
    ;
on_exception_or_overflow:
    on_opt exception_or_overflow  
        statement_list            
    | /* nothing */ 
    ;
exception_or_overflow:
    EXCEPTION
    | OVERFLOW_TOK
    ;
on_not_exception:
    NOT on_opt EXCEPTION     
        statement_list            
    | /* nothing */ 
    ;
on_overflow:
    on_opt OVERFLOW_TOK       
        statement_list            
    | /* nothing */
    ;
on_not_overflow:
    not_excep on_opt OVERFLOW_TOK   
        statement_list            
    | /* nothing */
    ;
    
invalid_key_opt:
    invalid_key_sentence 
    | not_invalid_key_sentence 
    | invalid_key_sentence not_invalid_key_sentence
        
    | 
    ;
invalid_key_sentence:
    INVALID key_opt  
    statement_list   
    ;
not_invalid_key_sentence:
    not_excep INVALID key_opt 
    statement_list            
    ;
not_excep:
    NOTEXCEP
    | NOT
    ;
string_with_pointer:
    with_opt POINTER name  
    | /* nothing */        
    ;
string_from_list:
    string_from             
    | string_from_list sep_opt string_from  
    | error 
    ;
string_from:
    gname   
    | gname DELIMITED by_opt delimited_by 
    ;
delimited_by:
    gname     
    | SIZE    
    | error 
    ;
end_stringcmd_opt:
    /* nothing */
    | END_STRINGCMD
    ;
end_unstring_opt:
    /* nothing */
    | END_UNSTRING
    ;

/* INSPECT statement */
inspect_statement:
    INSPECT
        name
        tallying_clause     
        replacing_clause    
    | INSPECT
        name
        converting_clause   
    ;
converting_clause:
        CONVERTING 
        noallname TO noallname inspect_before_after 
        ;
tallying_clause:
    TALLYING tallying_list 
    | /* nothing */        
    ;
tallying_list:
    tallying_list
        name FOR tallying_for_list  
    | /* nothing */     
    ;
tallying_for_list:
    tallying_for_list
        CHARACTERS inspect_before_after 
    | tallying_for_list
        ALL noallname inspect_before_after 
    | tallying_for_list
        LEADING noallname inspect_before_after 
    | /* nothing */     
    ;
replacing_clause:
    REPLACING
        replacing_list      
    | /* nothing */         
    ;
replacing_list:
    replacing_list
        CHARACTERS BY noallname inspect_before_after 
    | replacing_list
        replacing_kind replacing_by_list 
    | /* nothing */     
    ;
replacing_by_list:
    replacing_by_list
        noallname BY noallname inspect_before_after 
    | /* nothing */         
    ;
replacing_kind:
    ALL         
    | LEADING   
    | TRAILING  
    | FIRST     
    ;
inspect_before_after:
    inspect_before_after
        BEFORE initial_opt noallname
            
    | inspect_before_after
        AFTER initial_opt noallname
            
    | /* nothing */  
    ;
initial_opt:
    INITIAL_TOK
    | /* nothing */
    ;
    
expr:
    gname   
    | expr '*' expr 
    | expr '/' expr 
    | expr '+' expr 
    | expr '-' expr 
    | expr POW_OP expr 
    | '(' expr ')'  
    ;
/* expr_opt will be NULL or a (struct sym *) pointer if the expression
        was given, otherwise it will be valued -1 */
expr_opt:
        /* nothing */   
        | expr                  
        ;
using_options:
    /* nothing */   
    | USING     
      dummy     
      parm_list   /* modified to signal calling pgm */
    ;
returning_options:
    /* nothing */   
    | returning_options1 variable 
     
    ;
returning_options1: RETURNING 
    | GIVING 
    ;
dummy: /* nothing */ ;

procedure_division_using_chaining_opt:   /* defined at procedure division */
    /* nothing */         
    | USING gname_list    
    | CHAINING gname_list 
    ;

procedure_division_returning_opt:   /* defined at procedure division */
    /* nothing */        
    | RETURNING variable 
     
    ;

parm_list:
    parm_list sep_opt parameter
        
        | parameter
        
    ;
parameter:
    gname 
    
    /*| BY parm_type gname*/
    | by_opt parm_type gname
        
 /*   | OMITTED
         */
    ;
parm_type:
    REFERENCE 
    | VALUE 
    | CONTENT 
/*    | DESCRIPTOR */
    ;
intrinsic_parm_list:
    intrinsic_parm_list sep_opt intrinsic_parm
    
    | intrinsic_parm 
    ;
intrinsic_parm:
    gname 
    
    ;

perform_range: label perform_thru_opt
    
    ;

perform_options: perform_statements END_PERFORM 
    | gname TIMES
      
      perform_statements
      
      END_PERFORM 
    | with_test_opt UNTIL
      
      condition
      
      perform_statements
      
      END_PERFORM 
    | with_test_opt VARYING name FROM gname by_opt gname UNTIL
      
      condition
      
      perform_after_opt
      perform_statements
      END_PERFORM 
    | label perform_thru_opt
      
    | label perform_thru_opt with_test_opt UNTIL
      
      condition
      
    | label perform_thru_opt gname TIMES
      
    | label perform_thru_opt with_test_opt VARYING name
      FROM gname by_opt gname UNTIL
      condition
      
      perform_after_opt
    ;

perform_thru_opt: 
    /* nothing */ 
    | THRU label 
    ;
with_test_opt: 
    | with_opt TEST before_after
      
    ;
perform_after_opt:   /* nothing */ 
    | AFTER perform_after
     
    | AFTER perform_after AFTER perform_after
     
    | AFTER perform_after AFTER perform_after
      AFTER perform_after
     
    | AFTER perform_after AFTER perform_after
      AFTER perform_after AFTER perform_after
     
    | AFTER perform_after AFTER perform_after AFTER perform_after
      AFTER perform_after AFTER perform_after
     
    | AFTER perform_after AFTER perform_after AFTER perform_after
      AFTER perform_after AFTER perform_after AFTER perform_after
     
    ;
perform_after: name FROM gname
    by_opt gname UNTIL
    condition
    
    ;
perform_statements:  statement_list
/*
    |  CONTINUE 
*/
    ;
before_after:
    BEFORE  
    | AFTER 
    ;
condition:
    expr  extended_cond_op 
    
    expr_opt 
    
    | NOT condition 
    | condition 
      AND 
      
      implied_op_condition 
      
    | condition 
      OR      
      
      implied_op_condition 
      
    | '(' condition ')' 
    | cond_name 
     
    ;
implied_op_condition: 
    condition  
    | cond_op expr  
     
    | expr            
     
    ;
sign_condition:
      POSITIVE      
    | NEGATIVE      
    | ZERO          
    ;
class_condition:
    NUMERIC                  
    | ALPHABETIC_TOK         
    | ALPHABETIC_LOWER       
    | ALPHABETIC_UPPER       
    ;            
extended_cond_op:
    IS ext_cond               
    | IS NOT ext_cond         
    | IS ext_cond OR ext_cond 
    | ext_cond                
    | NOT is_opt ext_cond     
    | ext_cond OR ext_cond    
    ;
ext_cond:
    conditional       
    | class_condition 
    | sign_condition  
    ;
cond_op:
    conditional          
    | NOT conditional    
    | conditional OR conditional 
    ;
conditional:
    CONDITIONAL than_to_opt 
    ;
comma_opt: /* nothing */
    | ','
    ;
sep_opt:
    /* nothing */
    | LISTSEP
    ;
/* this token doesn't really exists, but forces look ahead 
   to keep line numbers synchronized with our position
   because we need to generate correct debug stabs */
/*
dummy_opt:
    / nothing /
    | TOKDUMMY
    ;
*/
key_opt:
        /* nothing */
        | KEY
        ;
advancing_opt:
    /* nothing */
    | ADVANCING
    ;
than_to_opt:
    /* nothing */
    | TO 
    | THAN 
    ;
record_opt:
    /* nothing */
    | RECORD
    ;
at_opt: /* nothing */
    | AT
    ;
in_opt: /* nothing */
    | IN
    ;
in_of:
    IN
    | OF
    ;
by_opt: /* nothing */
    | BY
    ;
with_opt:
    /* nothing */
    | WITH
    ;
on_opt:
    /* nothing */
    | ON
    ;
gname_opt:
    gname           
    | /* nothing */ 
    ;
to_opt: /* nothing */
    | TO 
    ;
name_var_list:
      name_var 
    | name_var_list sep_opt name_var
      
    ;
gname_list:
      gname 
    | gname_list sep_opt gname
      
    ;
gname: name 
    | gliteral  
    | FUNCTION LABELSTR '(' 
                intrinsic_parm_list ')' 
    | FUNCTION LABELSTR 
    ;
name_or_lit:
    name      
    | literal 
    ;
noallname:
    name      
    | without_all_literal 
    ;
gliteral:
    without_all_literal
    | all_literal
    ;
without_all_literal:
    literal             
    | special_literal   
    ;
all_literal:
    ALL literal 
    | ALL special_literal 
    ;
special_literal:
    SPACES          
    | ZERO          
    | QUOTES        
    | HIGHVALUES    
    | LOWVALUES     
    | NULL_TOK      
    ;
var_or_nliteral:
    variable        
    | nliteral      
    ;
nliteral:
    signed_nliteral 
    ;
literal:
    signed_nliteral 
    | CLITERAL      
    ;
signed_nliteral:
    NLITERAL        
    | '+' NLITERAL  
    | '-' NLITERAL
     
    ;
def_name_opt:
    def_name        
    | /* nothing */ 
    ;
def_name:
    STRING
    
    | FILLER    
    ;
variable_indexed:
    SUBSCVAR
    
    ;

/*variable:
    VARIABLE 
  | VARIABLE in_of subs_var 
        | LABELSTR 
    ; */
filename:
    literal 
    | STRING 
    ;
data_name:
    literal 
    | STRING 
    ;
cond_name:
    VARCOND '(' 
        subscripts  ')' 
    | VARCOND  
    ;
name:
    variable '(' gname ':' gname_opt ')' 
    | variable 
    | LABELSTR  
    ;
name_var:
      gname 
            ;
/*subs_var:
    variable
    | subscripted_variable
    ;
subscripted_variable:
    SUBSCVAR '(' 
        subscripts ')' 
        | SUBSCVAR in_of subs_var '(' 
        subscripts ')' 
        ;
*/
variable:
    qualified_var
    
    | qualified_var LPAR
     
     subscripts ')'
     
    ;
qualified_var:
    unqualified_var
    
    | unqualified_var in_of qualified_var
    
    ;
unqualified_var:
    VARIABLE
    
    | SUBSCVAR      
    ;
subscripts:
    subscript   
    | subscripts comma_opt subscript 
    ;
subscript:
    gname                   
    | subscript '+' gname   
    | subscript '-' gname   
    ;
integer:
    signed_nliteral
    
    ;
label:
    LABELSTR in_of LABELSTR
    | LABELSTR
    | NLITERAL 
    | NLITERAL in_of NLITERAL 
    | NLITERAL in_of LABELSTR 
    ;
anystring:
    STRING
        | LABELSTR
    ;
%%
