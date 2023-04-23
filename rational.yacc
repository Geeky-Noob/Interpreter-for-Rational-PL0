open DataTypes ; 

exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ; 

%%
%name My

%term LBRACE | RBRACE | LPAREN | RPAREN |EOF | 
      IF | ELSE | THEN | FI | WHILE | DO | OD | 
      SEMICOLON | COMMA | INTEGER | BOOL | RATIONAL|NEG|
      LT_EQ | LT | NOT_EQ | EQ | GT | GT_EQ | INVERSE|MAKE_RAT|RATI|SHOW_RAT|SHOW_DECI|FROM_DECI|TO_DECI|
      PLUS_INT | MINUS_INT | TIMES_INT | DIV_INT | MOD_INT |PLUS_RAT | MINUS_RAT | TIMES_RAT | DIV_RAT| 
      NOT | AND | OR | TRUE | FALSE | PROCEDURE| PRINT|
      ASSIGN | READ | CALL| BIGINT of string| RAT of string | ID of string

%nonterm start of AST | block of BLK |
         comseq of CMD list  |commands of CMD list |
          dec of DEC| vardecseq of VARDEC list|
           vardec of VARDEC| prodecseq of PRODEC list| prodec of PRODEC| 
         command of CMD  |
         typedec of Type   |
         varlist of string list |
         expression of Type * Exp 


%eop EOF
%noshift EOF
%pos int 
%verbose 

%right ASSIGN
%left OR
%left AND
%left EQ NOT_EQ
%left GT GT_EQ LT LT_EQ
%left PLUS_INT MINUS_INT PLUS_RAT MINUS_RAT
%left TIMES_INT MOD_INT DIV_INT TIMES_RAT DIV_RAT 
%right NOT INVERSE NEG
%left LPAREN RPAREN

%%

start: block (PROG(block))
block: dec comseq (BLK(dec, comseq))
comseq: LBRACE commands RBRACE ((commands))
commands: (([]))
        | command SEMICOLON commands ((command::commands))
dec: vardecseq prodecseq((DEC(vardecseq,prodecseq)))
vardecseq: (([]))
        | vardec vardecseq ((vardec::vardecseq))
prodecseq: (([]))
        | prodec prodecseq ((prodec::prodecseq))
vardec: typedec varlist SEMICOLON (VARDEC(varlist , typedec))
prodec: PROCEDURE ID block SEMICOLON (PRODEC(ID, block))
typedec: INTEGER((BigInt))
        | RATIONAL ((Rational))
        | BOOL((Bool))
varlist:  ID (([ID]))
        | ID COMMA varlist ((ID::varlist))
command: READ LPAREN ID RPAREN ((Read(ID)))
        | PRINT LPAREN expression RPAREN (Print(expression))
        | CALL ID ((Call(ID)))
        | ID ASSIGN expression ((SET(ID, #2 expression)))
        | IF expression THEN comseq ELSE comseq FI ((ITE(#2 expression, comseq1, comseq2)))
        | WHILE expression DO comseq OD ((WH( #2 expression , comseq )))
expression: expression LT_EQ expression ((Bool , LT_EQ(#2 expression1 , #2 expression2)))
            | expression LT expression ((Bool , LT(#2 expression1 , #2 expression2)))
            | expression EQ expression ((Bool , EQ(#2 expression1 , #2 expression2)))
            | expression NOT_EQ expression ((Bool , NOT_EQ(#2 expression1 , #2 expression2)))
            | expression GT expression ((Bool , GT(#2 expression1 , #2 expression2)))
            | expression GT_EQ expression ((Bool , GT_EQ(#2 expression1 , #2 expression2)))
            | expression PLUS_INT expression ((BigInt , PLUS_INT(#2 expression1 , #2 expression2)))
            | expression MINUS_INT expression ((BigInt , MINUS_INT(#2 expression1 , #2 expression2)))
            | expression TIMES_INT expression ((BigInt , TIMES_INT(#2 expression1 , #2 expression2)))
            | expression DIV_INT expression ((BigInt , DIV_INT(#2 expression1 , #2 expression2)))
            | expression MOD_INT expression ((BigInt , MOD_INT(#2 expression1 , #2 expression2)))
            | expression PLUS_RAT expression ((Rational , PLUS_RAT(#2 expression1 , #2 expression2)))
            | expression MINUS_RAT expression ((Rational , MINUS_RAT(#2 expression1 , #2 expression2)))
            | expression TIMES_RAT expression ((Rational , TIMES_RAT(#2 expression1 , #2 expression2)))
            | expression DIV_RAT expression ((Rational , DIV_RAT(#2 expression1 , #2 expression2)))
            | INVERSE expression ((Rational , INVERSE(#2 expression)))
            | MAKE_RAT LPAREN expression COMMA expression RPAREN ((Rational , MAKE_RAT(#2 expression1, #2 expression2)))
            | RATI LPAREN expression RPAREN ((Rational , RATI(#2 expression)))
            | SHOW_RAT LPAREN expression RPAREN ((Rational , SHOW_RAT(#2 expression)))
            | SHOW_DECI LPAREN expression RPAREN ((Rational , SHOW_DECI(#2 expression)))
            | FROM_DECI LPAREN expression RPAREN ((Rational , FROM_DECI(#2 expression)))
            | TO_DECI LPAREN expression RPAREN ((Rational , TO_DECI(#2 expression)))
            | expression AND expression ((Bool, AND(#2 expression1 , #2 expression2)))
            | expression OR expression ((Bool, OR(#2 expression1 , #2 expression2)))
            | LPAREN expression RPAREN ((expression))
            | TRUE ((Bool , TT ))
            | FALSE ((Bool , FF ))
            | ID ((String, Ident(ID)))
            | BIGINT ((BigInt , Big_int(BIGINT) ))
            | RAT ((Rational, Rat(RAT)))
            | NOT expression ((Bool, NOT(#2 expression)))
            | NEG expression ((#1 expression, NEG(#2 expression)))



