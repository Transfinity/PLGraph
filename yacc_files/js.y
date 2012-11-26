%token AssignmentOperator
%token EqualityOperator
%token FloatingPointLiteral
%token Identifier
%token IncrementOperator
%token IntegerLiteral
%token MultiplicativeOperator
%token RelationalOperator
%token ShiftOperator
%token StringLiteral
%token UnaryOperator

%%

Program:
          /* Empty */
          | Element Program
;

Element:
          "function" Identifier '(' ParameterListOpt ')' CompoundStatement
          | Statement
;

ParameterListOpt:
          /* Empty */
          | ParameterList
;

ParameterList:
          Identifier
          | Identifier ',' ParameterList
;

CompoundStatement:
          '{' Statements '}'
;

Statements:
          /* Empty */
          | Statement Statements
;

Statement:
          ';'
          | "if" Condition Statement
          | "if" Condition Statement "else" Statement
          | "while" Condition Statement
          | ForParen ';' ExpressionOpt ';' ExpressionOpt ')' Statement
          | ForBegin ';' ExpressionOpt ';' ExpressionOpt ')' Statement
          | ForBegin "in" Expression ')' Statement
          | "break" ';'
          | "continue" ';'
          | "with" '(' Expression ')' Statement
          | "return" ExpressionOpt ';'
          | CompoundStatement
          | VariablesOrExpression ';'
;

Condition:
          '(' Expression ')'
;

ForParen:
          "for" '('
;

ForBegin:
          ForParen VariablesOrExpression
;

VariablesOrExpression:
          "var" Variables
          | Expression
;

Variables:
          Variable
          | Variable ',' Variables
;

Variable:
          Identifier
          | Identifier '=' AssignmentExpression
;

ExpressionOpt:
          /* Empty */
          | Expression
;

Expression:
          AssignmentExpression
          | AssignmentExpression ',' Expression
;

AssignmentExpression:
          ConditionalExpression
          | ConditionalExpression AssignmentOperator AssignmentExpression
;

ConditionalExpression:
          OrExpression
          | OrExpression '?' AssignmentExpression ':' AssignmentExpression
;

OrExpression:
          AndExpression
          | AndExpression "||" OrExpression
;

AndExpression:
          BitwiseOrExpression
          | BitwiseOrExpression "&&" AndExpression
;

BitwiseOrExpression:
          BitwiseXorExpression
          | BitwiseXorExpression '|' BitwiseOrExpression
;

BitwiseXorExpression:
          BitwiseAndExpression
          | BitwiseAndExpression '^' BitwiseXorExpression
;

BitwiseAndExpression:
          EqualityExpression
          | EqualityExpression '&' BitwiseAndExpression
;

EqualityExpression:
          RelationalExpression
          | RelationalExpression EqualityOperator EqualityExpression
;

RelationalExpression:
          ShiftExpression
          | RelationalExpression RelationalOperator ShiftExpression
;

ShiftExpression:
          AdditiveExpression
          | AdditiveExpression ShiftOperator ShiftExpression
;

AdditiveExpression:
          MultiplicativeExpression
          | MultiplicativeExpression '+' AdditiveExpression
          | MultiplicativeExpression '-' AdditiveExpression
;

MultiplicativeExpression:
          UnaryExpression
          | UnaryExpression MultiplicativeOperator MultiplicativeExpression
;

UnaryExpression:
          MemberExpression
          | UnaryOperator UnaryExpression
          | '-' UnaryExpression
          | IncrementOperator MemberExpression
          | MemberExpression IncrementOperator
          | "new" Constructor
          | "delete" MemberExpression
;

Constructor:
          "this" '.' ConstructorCall
          | ConstructorCall
;

ConstructorCall:
          Identifier
          | Identifier '(' ArgumentListOpt ')'
          | Identifier '.' ConstructorCall
;

MemberExpression:
          PrimaryExpression
          | PrimaryExpression '.' MemberExpression
          | PrimaryExpression '[' Expression ']'
          | PrimaryExpression '(' ArgumentListOpt ')'
;

ArgumentListOpt:
          /* Empty */
          | ArgumentList
;

ArgumentList:
          AssignmentExpression
          | AssignmentExpression ',' ArgumentList
;

PrimaryExpression:
          '(' Expression ')'
          | Identifier
          | IntegerLiteral
          | FloatingPointLiteral
          | StringLiteral
          | "false"
          | "true"
          | "null"
          | "this"
          ;
