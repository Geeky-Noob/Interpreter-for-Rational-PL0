structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val linep = ref 1;
val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

val badCh : int * char -> unit = fn
                 (l1,ch) => TextIO.output(TextIO.stdOut,"lex:line " ^Int.toString l1^": Invalid character "^Int.toString(ord ch)^"="^str(ch)^"\n")

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
%s COMMENT;
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%
<INITIAL>\n       => ( pos:= (!pos) + 1; lex()); 
<INITIAL>{ws}+    => (lex());
<INITIAL>"(*"  => (YYBEGIN COMMENT;lex());
<COMMENT>"*)"  => (YYBEGIN INITIAL;lex());
<INITIAL>"tt"     => (Tokens.TRUE(!pos, !pos ));
<INITIAL>"ff"     => (Tokens.FALSE(!pos, !pos ));
<INITIAL>"("      => (Tokens.LPAREN(!pos, !pos ));
<INITIAL>")"      => (Tokens.RPAREN(!pos, !pos )); 
<INITIAL>"}"      => (Tokens.RBRACE(!pos, !pos )); 
<INITIAL>"{"      => (Tokens.LBRACE(!pos, !pos )); 
<INITIAL>"!"      => (Tokens.NOT(!pos, !pos )) ; 
<INITIAL>";"       => (Tokens.SEMICOLON(!pos, !pos ));
<INITIAL>":="       => (Tokens.ASSIGN(!pos, !pos ));
<INITIAL>","       => (Tokens.COMMA(!pos, !pos ));
<INITIAL>"+"       => (Tokens.PLUS_INT(!pos, !pos ));
<INITIAL>"-"       => (Tokens.MINUS_INT(!pos, !pos ));
<INITIAL>"*"       => (Tokens.TIMES_INT(!pos, !pos ));
<INITIAL>"/"       => (Tokens.DIV_INT(!pos, !pos ));
<INITIAL>"%"       => (Tokens.MOD_INT(!pos, !pos ));
<INITIAL>"~"      => (Tokens.NEG(!pos, !pos )) ; 
<INITIAL>".+."       => (Tokens.PLUS_RAT(!pos, !pos ));
<INITIAL>".-."       => (Tokens.MINUS_RAT(!pos, !pos ));
<INITIAL>".*."       => (Tokens.TIMES_RAT(!pos, !pos ));
<INITIAL>"./."       => (Tokens.DIV_RAT(!pos, !pos ));
<INITIAL>"inverse"       => (Tokens.INVERSE(!pos, !pos ));
<INITIAL>"&&"       => ( Tokens.AND(!pos, !pos )); 
<INITIAL>"||"       => (Tokens.OR(!pos, !pos ));
<INITIAL>"read"       => (Tokens.READ(!pos, !pos));
<INITIAL>"procedure" => (Tokens.PROCEDURE(!pos, !pos));
<INITIAL>"call" => (Tokens.CALL(!pos, !pos));
<INITIAL>"print"    => (Tokens.PRINT(!pos, !pos));
<INITIAL>"if"       => (Tokens.IF(!pos, !pos )) ;
<INITIAL>"then"       => (Tokens.THEN(!pos, !pos ));
<INITIAL>"else" => (Tokens.ELSE(!pos, !pos )) ; 
<INITIAL>"fi"       => (Tokens.FI(!pos, !pos )) ;
<INITIAL>"while"       => (Tokens.WHILE(!pos, !pos ));
<INITIAL>"do"       => (Tokens.DO(!pos, !pos ));
<INITIAL>"od"       => (Tokens.OD(!pos, !pos ));
<INITIAL>"integer"       => (Tokens.INTEGER(!pos, !pos ));
<INITIAL>"boolean"       => (Tokens.BOOL(!pos, !pos ));
<INITIAL>"rational"       => (Tokens.RATIONAL(!pos, !pos ));
<INITIAL>"make_rat"  => (Tokens.MAKE_RAT(!pos, !pos ));
<INITIAL>"rat"       => (Tokens.RATI(!pos, !pos ));
<INITIAL>"showRat"       => (Tokens.SHOW_RAT(!pos, !pos ));
<INITIAL>"showDecimal"       => (Tokens.SHOW_DECI(!pos, !pos ));
<INITIAL>"fromDecimal"       => (Tokens.FROM_DECI(!pos, !pos ));
<INITIAL>"toDecimal"         => (Tokens.TO_DECI(!pos, !pos));
<INITIAL>"<="       => (Tokens.LT_EQ(!pos, !pos ));
<INITIAL>">="       => (Tokens.GT_EQ(!pos, !pos ));
<INITIAL>">"       => (Tokens.GT(!pos, !pos ));
<INITIAL>"<"       => (Tokens.LT(!pos, !pos ));
<INITIAL>"="       => (Tokens.EQ(!pos, !pos ));
<INITIAL>"<>"       => (Tokens.NOT_EQ(!pos, !pos ));
<INITIAL>{alpha}({alpha} | {digit})* => (Tokens.ID( yytext ,!pos ,!pos ));
<INITIAL>([~]?)({digit}){digit}* => (Tokens.BIGINT( yytext ,!pos ,!pos));
<INITIAL>"~"{digit}*"\."{digit}*"("{digit}+")" | "+"{digit}*"\."{digit}*"("{digit}+")" | {digit}*"\."{digit}*"("{digit}+")" => (Tokens.RAT( yytext ,!pos ,!pos));
<COMMENT> .          => (lex());
<COMMENT> \n          => (lex());