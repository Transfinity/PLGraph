/*    perly.y
 *
 *    Copyright (c) 1991-2002, 2003, 2004, 2005, 2006 Larry Wall
 *    Copyright (c) 2007, 2008, 2009, 2010, 2011 by Larry Wall and others
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

/*
 * 'I see,' laughed Strider.  'I look foul and feel fair.  Is that it?
 *  All that is gold does not glitter, not all those who wander are lost.'
 *
 *     [p.171 of _The Lord of the Rings_, I/x: "Strider"]
 */

/*
 * This file holds the grammar for the Perl language. If edited, you need
 * to run regen_perly.pl, which re-creates the files perly.h, perly.tab
 * and perly.act which are derived from this.
 *
 * Note that these derived files are included and compiled twice; once
 * from perly.c, and once from madly.c. The second time, a number of MAD
 * macros are defined, which compile in extra code that allows the parse
 * tree to be accurately dumped. In particular:
 *
 * MAD            defined if compiling madly.c
 * DO_MAD(A)      expands to A  under madly.c, to null otherwise
 * IF_MAD(a,b)    expands to A under madly.c, to B otherwise
 * TOKEN_GETMAD() expands to token_getmad() under madly.c, to null otherwise
 * TOKEN_FREE()   similarly
 * OP_GETMAD()    similarly
 * IVAL(i)        expands to (i)->tk_lval.ival or (i)
 * PVAL(p)        expands to (p)->tk_lval.pval or (p)
 *
 * The main job of of this grammar is to call the various newFOO()
 * functions in op.c to build a syntax tree of OP structs.
 * It relies on the lexer in toke.c to do the tokenizing.
 *
 * Note: due to the way that the cleanup code works WRT to freeing ops on
 * the parse stack, it is dangerous to assign to the $n variables within
 * an action.
 */

/*  Make the parser re-entrant. */
%{
%}

%pure_parser

/* FIXME for MAD - is the new mintro on while and until important?  */

%start grammar

%token <ival> GRAMPROG GRAMEXPR GRAMBLOCK GRAMBARESTMT GRAMFULLSTMT GRAMSTMTSEQ

%token <i_tkval> '{' '}' '[' ']' '-' '+' '$' '@' '%' '*' '&' ';'

%token <opval> WORD METHOD FUNCMETH THING PMFUNC PRIVATEREF QWLIST
%token <opval> FUNC0OP FUNC0SUB UNIOPSUB LSTOPSUB
%token <opval> PLUGEXPR PLUGSTMT
%token <p_tkval> LABEL
%token <i_tkval> FORMAT SUB ANONSUB PACKAGE USE
%token <i_tkval> WHILE UNTIL IF UNLESS ELSE ELSIF CONTINUE FOR
%token <i_tkval> GIVEN WHEN DEFAULT
%token <i_tkval> LOOPEX DOTDOT YADAYADA
%token <i_tkval> FUNC0 FUNC1 FUNC UNIOP LSTOP
%token <i_tkval> RELOP EQOP MULOP ADDOP
%token <i_tkval> DOLSHARP DO HASHBRACK NOAMP
%token <i_tkval> LOCAL MY MYSUB REQUIRE
%token <i_tkval> COLONATTR


%nonassoc <i_tkval> PREC_LOW
%nonassoc LOOPEX

%left <i_tkval> OROP DOROP
%left <i_tkval> ANDOP
%right <i_tkval> NOTOP
%nonassoc LSTOP LSTOPSUB
%left <i_tkval> ','
%right <i_tkval> ASSIGNOP
%right <i_tkval> '?' ':'
%nonassoc DOTDOT YADAYADA
%left <i_tkval> OROR DORDOR
%left <i_tkval> ANDAND
%left <i_tkval> BITOROP
%left <i_tkval> BITANDOP
%nonassoc EQOP
%nonassoc RELOP
%nonassoc UNIOP UNIOPSUB
%nonassoc REQUIRE
%left <i_tkval> SHIFTOP
%left ADDOP
%left MULOP
%left <i_tkval> MATCHOP
%right <i_tkval> '!' '~' UMINUS REFGEN
%right <i_tkval> POWOP
%nonassoc <i_tkval> PREINC PREDEC POSTINC POSTDEC
%left <i_tkval> ARROW
%nonassoc <i_tkval> ')'
%left <i_tkval> '('
%left '[' '{'

%token <i_tkval> PEG

%%

/* Top-level choice of what kind of thing yyparse was called to parse */
grammar	:	GRAMPROG
			
		remember stmtseq
			
	|	GRAMEXPR
			
		optexpr
			
	|	GRAMBLOCK
			
		block
			
	|	GRAMBARESTMT
			
		barestmt
			
	|	GRAMFULLSTMT
			
		fullstmt
			
	|	GRAMSTMTSEQ
			
		stmtseq
			
	;

/* An ordinary block */
block	:	'{' remember stmtseq '}'
	;

remember:	/* NULL */	/* start a full lexical scope */
			
	;

mydefsv:	/* NULL */	/* lexicalize $_ */
			
	;

mblock	:	'{' mremember stmtseq '}'
	;

mremember:	/* NULL */	/* start a partial lexical scope */
			
	;

/* A sequence of statements in the program */
stmtseq	:	/* NULL */
			
	|	stmtseq fullstmt
			
	;

/* A statement in the program, including optional labels */
fullstmt:	barestmt
			
	|	labfullstmt
			
	;

labfullstmt:	LABEL barestmt
			
	|	LABEL labfullstmt
			
	;

/* A bare statement, lacking label and other aspects of state op */
barestmt:	PLUGSTMT
			
	|	PEG
			
	|	FORMAT startformsub formname block
			
	|	SUB startsub subname proto subattrlist subbody
			
	|	MYSUB startsub subname proto subattrlist subbody
			
	|	PACKAGE WORD WORD ';'
			
	|	USE startsub
			
		WORD WORD optlistexpr ';'
			
	|	IF lpar_or_qw remember mexpr ')' mblock else
			
	|	UNLESS lpar_or_qw remember miexpr ')' mblock else
			
	|	GIVEN lpar_or_qw remember mydefsv mexpr ')' mblock
			
	|	WHEN lpar_or_qw remember mexpr ')' mblock
			
	|	DEFAULT block
			
	|	WHILE lpar_or_qw remember texpr ')' mintro mblock cont
			
	|	UNTIL lpar_or_qw remember iexpr ')' mintro mblock cont
			
	|	FOR lpar_or_qw remember mnexpr ';' texpr ';' mintro mnexpr ')'
		mblock
			
	|	FOR MY remember my_scalar lpar_or_qw mexpr ')' mblock cont
			
	|	FOR scalar lpar_or_qw remember mexpr ')' mblock cont
			
	|	FOR lpar_or_qw remember mexpr ')' mblock cont
			
	|	block cont
			
	|	PACKAGE WORD WORD '{' remember
			
		stmtseq '}'
	|	sideff ';'
			
	|	';'
			
	;

/* An expression which may have a side-effect */
sideff	:	error
			
	|	expr
			
	|	expr IF expr
			
	|	expr UNLESS expr
			
	|	expr WHILE expr
			
	|	expr UNTIL iexpr
			
	|	expr FOR expr
			
	|	expr WHEN expr
			
	;

/* else and elsif blocks */
else	:	/* NULL */
			
	|	ELSE mblock
			
	|	ELSIF lpar_or_qw mexpr ')' mblock else
			
	;

/* Continue blocks */
cont	:	/* NULL */
			
	|	CONTINUE block
			
	;

/* determine whether there are any new my declarations */
mintro	:	/* NULL */
			

/* Normal expression */
nexpr	:	/* NULL */
			
	|	sideff
	;

/* Boolean expression */
texpr	:	/* NULL means true */
			
	|	expr
	;

/* Inverted boolean expression */
iexpr	:	expr
			
	;

/* Expression with its own lexical scope */
mexpr	:	expr
			
	;

mnexpr	:	nexpr
			
	;

miexpr	:	iexpr
			
	;

formname:	WORD		
	|	/* NULL */	
	;

startsub:	/* NULL */	/* start a regular subroutine scope */
			

	;

startanonsub:	/* NULL */	/* start an anonymous subroutine scope */
			
	;

startformsub:	/* NULL */	/* start a format subroutine scope */
			
	;

/* Name of a subroutine - must be a bareword, could be special */
subname	:	WORD	
	;

/* Subroutine prototype */
proto	:	/* NULL */
			
	|	THING
	;

/* Optional list of subroutine attributes */
subattrlist:	/* NULL */
			
	|	COLONATTR THING
			
	|	COLONATTR
			
	;

/* List of attributes for a "my" variable declaration */
myattrlist:	COLONATTR THING
			
	|	COLONATTR
			
	;

/* Subroutine body - either null or a block */
subbody	:	block	
	|	';'	
	;

/* Ordinary expressions; logical combinations */
expr	:	expr ANDOP expr
			
	|	expr OROP expr
			
	|	expr DOROP expr
			
	|	listexpr %prec PREC_LOW
	;

/* Expressions are a list of terms joined by commas */
listexpr:	listexpr ','
			
	|	listexpr ',' term
			
	|	term %prec PREC_LOW
	;

/* List operators */
listop	:	LSTOP indirob listexpr /* map  @args or print $fh @args */
			
	|	FUNC '(' indirob expr ')'      /* print ($fh @args */
			
	|	term ARROW method lpar_or_qw optexpr ')' /* $foo->bar(list) */
			
	|	term ARROW method                     /* $foo->bar */
			
	|	METHOD indirob optlistexpr           /* new Class @args */
			
	|	FUNCMETH indirob '(' optexpr ')'    /* method $object (@args) */
			
	|	LSTOP optlistexpr                    /* print @args */
			
	|	FUNC '(' optexpr ')'                 /* print (@args) */
			
	|	LSTOPSUB startanonsub block /* sub f(&@);   f  ... */
			
		    optlistexpr		%prec LSTOP  /* ... @bar */
			
	;

/* Names of methods. May use $object->$methodname */
method	:	METHOD
	|	scalar
	;

/* Some kind of subscripted expression */
subscripted:    star '{' expr ';' '}'        /* *main:: */
                        /* In this and all the hash accessors, ';' is
                         * provided by the tokeniser */
	|	scalar '[' expr ']'          /* $array[$element] */
			
	|	term ARROW '[' expr ']'      /* somearef->[$element] */
			
	|	subscripted '[' expr ']'    /* $foo->[$bar]->[$baz] */
			
	|	scalar '{' expr ';' '}'    /* $foo */
	|	term ARROW '{' expr ';' '}' /* somehref-> */
	|	subscripted '{' expr ';' '}' /* $foo->[bar]-> */
	|	term ARROW '(' ')'          /* $subref->() */
			
	|	term ARROW '(' expr ')'     /* $subref->(@args) */
			

	|	subscripted lpar_or_qw expr ')'   /* $foo->->(@args) */
			
	|	subscripted lpar_or_qw ')'        /* $foo->->() */
			
	|	'(' expr ')' '[' expr ']'            /* list slice */
			
	|	QWLIST '[' expr ']'            /* list literal slice */
			
	|	'(' ')' '[' expr ']'                 /* empty list slice! */
			
    ;

/* Binary operators between terms */
termbinop:	term ASSIGNOP term                     /* $x = $y */
			
	|	term POWOP term                        /* $x ** $y */
			
	|	term MULOP term                        /* $x * $y, $x x $y */
			
	|	term ADDOP term                        /* $x + $y */
			
	|	term SHIFTOP term                      /* $x >> $y, $x << $y */
			
	|	term RELOP term                        /* $x > $y, etc. */
			
	|	term EQOP term                         /* $x == $y, $x eq $y */
			
	|	term BITANDOP term                     /* $x & $y */
			
	|	term BITOROP term                      /* $x | $y */
			
	|	term DOTDOT term                       /* $x..$y, $x...$y */
			
	|	term ANDAND term                       /* $x && $y */
			
	|	term OROR term                         /* $x || $y */
			
	|	term DORDOR term                       /* $x // $y */
			
	|	term MATCHOP term                      /* $x =~ /$y/ */
			
    ;

/* Unary operators and terms */
termunop : '-' term %prec UMINUS                       /* -$x */
			
	|	'+' term %prec UMINUS                  /* +$x */
			
	|	'!' term                               /* !$x */
			
	|	'~' term                               /* ~$x */
			
	|	term POSTINC                           /* $x++ */
			
	|	term POSTDEC                           /* $x-- */
			
	|	PREINC term                            /* ++$x */
			
	|	PREDEC term                            /* --$x */
			

    ;

/* Constructors for anonymous data */
anonymous:	'[' expr ']'
			
	|	'[' ']'
			
	|	HASHBRACK expr ';' '}'	%prec '(' /*  */
	|	HASHBRACK ';' '}'	%prec '(' /*  (';' by tokener) */
	|	ANONSUB startanonsub proto subattrlist block	%prec '('
			

    ;

/* Things called with "do" */
termdo	:       DO term	%prec UNIOP                     /* do $filename */
			
	|	DO block	%prec '('               /* do { code */
			
	|	DO WORD lpar_or_qw ')'                  /* do somesub() */
			
	|	DO WORD lpar_or_qw expr ')'             /* do somesub(@args) */
			
	|	DO scalar lpar_or_qw ')'                /* do $subref () */
			
	|	DO scalar lpar_or_qw expr ')'           /* do $subref (@args) */
			

        ;

term	:	termbinop
	|	termunop
	|	anonymous
	|	termdo
	|	term '?' term ':' term
			
	|	REFGEN term                          /* \$x, \@y, \%z */
			
	|	myattrterm	%prec UNIOP
			
	|	LOCAL term	%prec UNIOP
			
	|	'(' expr ')'
			
	|	QWLIST
			
	|	'(' ')'
			
	|	scalar	%prec '('
			
	|	star	%prec '('
			
	|	hsh 	%prec '('
			
	|	ary 	%prec '('
			
	|	arylen 	%prec '('                    /* $#x, $# */
			
	|       subscripted
			
	|	ary '[' expr ']'                     /* array slice */
			
	|	ary '{' expr ';' '}'                 /* @hash */
	|	THING	%prec '('
			
	|	amper                                /* &foo; */
			
	|	amper lpar_or_qw ')'                 /* &foo() */
			
	|	amper lpar_or_qw expr ')'            /* &foo(@args) */
			
	|	NOAMP WORD optlistexpr               /* foo(@args) */
			
	|	LOOPEX  /* loop exiting command (goto, last, dump, etc) */
			
	|	LOOPEX term
			
	|	NOTOP listexpr                       /* not $foo */
			
	|	UNIOP                                /* Unary op, $_ implied */
			
	|	UNIOP block                          /* eval * */
			
	|	UNIOP term                           /* Unary op */
			
	|	REQUIRE                              /* require, $_ implied */
			
	|	REQUIRE term                         /* require Foo */
			
	|	UNIOPSUB
			
	|	UNIOPSUB term                        /* Sub treated as unop */
			
	|	FUNC0                                /* Nullary operator */
			
	|	FUNC0 '(' ')'
			
	|	FUNC0OP       /* Same as above, but op created in toke.c */
			
	|	FUNC0OP '(' ')'
			
	|	FUNC0SUB                             /* Sub treated as nullop */
			
	|	FUNC1 '(' ')'                        /* not () */
			
	|	FUNC1 '(' expr ')'                   /* not($foo) */
			
	|	PMFUNC '(' listexpr ')'		/* m//, s///, tr/// */
			
	|	WORD
	|	listop
	|	YADAYADA
			
	|	PLUGEXPR
	;

/* "my" declarations, with optional attributes */
myattrterm:	MY myterm myattrlist
			
	|	MY myterm
			
	;

/* Things that can be "my"'d */
myterm	:	'(' expr ')'
			
	|	'(' ')'
			
	|	scalar	%prec '('
			
	|	hsh 	%prec '('
			
	|	ary 	%prec '('
			
	;

/* Basic list expressions */
optlistexpr:	/* NULL */ %prec PREC_LOW
			
	|	listexpr    %prec PREC_LOW
			
	;

optexpr:	/* NULL */
			
	|	expr
			
	;

lpar_or_qw:	'('
			
	|	QWLIST
			
		'('
			
	;

/* A little bit of trickery to make "for my $foo (@bar)" actually be
   lexical */
my_scalar:	scalar
			
	;

amper	:	'&' indirob
			
	;

scalar	:	'$' indirob
			
	;

ary	:	'@' indirob
			
	;

hsh	:	'%' indirob
			
	;

arylen	:	DOLSHARP indirob
			
	;

star	:	'*' indirob
			
	;

/* Indirect objects */
indirob	:	WORD
			
	|	scalar %prec PREC_LOW
			
	|	block
			

	|	PRIVATEREF
			
	;
