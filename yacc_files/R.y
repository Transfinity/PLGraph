%{
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2010  The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */
%}


%token		END_OF_INPUT ERROR
%token		STR_CONST NUM_CONST NULL_CONST SYMBOL FUNCTION 
%token		INCOMPLETE_STRING
%token		LEFT_ASSIGN EQ_ASSIGN RIGHT_ASSIGN LBB
%token		FOR IN IF ELSE WHILE NEXT BREAK REPEAT
%token		GT GE LT LE EQ NE AND OR AND2 OR2
%token		NS_GET NS_GET_INT

/* This is the precedence table, low to high */
%left		'?'
%left		LOW WHILE FOR REPEAT
%right		IF
%left		ELSE
%right		LEFT_ASSIGN
%right		EQ_ASSIGN
%left		RIGHT_ASSIGN
%left		'~' TILDE
%left		OR OR2
%left		AND AND2
%left		UNOT NOT
%nonassoc   	GT GE LT LE EQ NE
%left		'+' '-'
%left		'*' '/'
%left		SPECIAL
%left		':'
%left		UMINUS UPLUS
%right		'^'
%left		'$' '@'
%left		NS_GET NS_GET_INT
%nonassoc	'(' '[' LBB

%%

prog	:	END_OF_INPUT			
	|	'\n'				
	|	expr_or_assign '\n'			
	|	expr_or_assign ';'			
	|	error	 			
	;

expr_or_assign  :    expr                       
                |    equal_assign               
                ;

equal_assign    :    expr EQ_ASSIGN expr_or_assign              
                ;

expr	: 	NUM_CONST			
	|	STR_CONST			
	|	NULL_CONST			
	|	SYMBOL				

	|	'{' exprlist '}'		
	|	'(' expr_or_assign ')'			

	|	'-' expr %prec UMINUS		
	|	'+' expr %prec UMINUS		
	|	'!' expr %prec UNOT		
	|	'~' expr %prec TILDE		
	|	'?' expr			

	|	expr ':'  expr			
	|	expr '+'  expr			
	|	expr '-' expr			
	|	expr '*' expr			
	|	expr '/' expr			
	|	expr '^' expr 			
	|	expr SPECIAL expr		
	|	expr '%' expr			
	|	expr '~' expr			
	|	expr '?' expr			
	|	expr LT expr			
	|	expr LE expr			
	|	expr EQ expr			
	|	expr NE expr			
	|	expr GE expr			
	|	expr GT expr			
	|	expr AND expr			
	|	expr OR expr			
	|	expr AND2 expr			
	|	expr OR2 expr			

	|	expr LEFT_ASSIGN expr 		
	|	expr RIGHT_ASSIGN expr 		
	|	FUNCTION '(' formlist ')' cr expr_or_assign %prec LOW
						
	|	expr '(' sublist ')'		
	|	IF ifcond expr_or_assign 			
	|	IF ifcond expr_or_assign ELSE expr_or_assign	
	|	FOR forcond expr_or_assign %prec FOR 	
	|	WHILE cond expr_or_assign			
	|	REPEAT expr_or_assign			
	|	expr LBB sublist ']' ']'	
	|	expr '[' sublist ']'		
	|	SYMBOL NS_GET SYMBOL		
	|	SYMBOL NS_GET STR_CONST		
	|	STR_CONST NS_GET SYMBOL		
	|	STR_CONST NS_GET STR_CONST	
	|	SYMBOL NS_GET_INT SYMBOL	
	|	SYMBOL NS_GET_INT STR_CONST	
	|	STR_CONST NS_GET_INT SYMBOL	
	|	STR_CONST NS_GET_INT STR_CONST	
	|	expr '$' SYMBOL			
	|	expr '$' STR_CONST		
	|	expr '@' SYMBOL			
	|	expr '@' STR_CONST		
	|	NEXT				
	|	BREAK				
	;


cond	:	'(' expr ')'			
	;

ifcond	:	'(' expr ')'			
	;

forcond :	'(' SYMBOL IN expr ')' 		
	;


exprlist:					
	|	expr_or_assign			
	|	exprlist ';' expr_or_assign	
	|	exprlist ';'			
	|	exprlist '\n' expr_or_assign	
	|	exprlist '\n'			
	;

sublist	:	sub				
	|	sublist cr ',' sub		
	;

sub	:					
	|	expr				
	|	SYMBOL EQ_ASSIGN 			
	|	SYMBOL EQ_ASSIGN expr			
	|	STR_CONST EQ_ASSIGN 			
	|	STR_CONST EQ_ASSIGN expr		
	|	NULL_CONST EQ_ASSIGN 			
	|	NULL_CONST EQ_ASSIGN expr		
	;

formlist:					
	|	SYMBOL				
	|	SYMBOL EQ_ASSIGN expr			
	|	formlist ',' SYMBOL		
	|	formlist ',' SYMBOL EQ_ASSIGN expr	
	;

cr	:					
	;
%%
