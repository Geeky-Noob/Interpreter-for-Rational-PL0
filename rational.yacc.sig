signature My_TOKENS =
sig
type ('a,'b) token
type svalue
val ID: (string) *  'a * 'a -> (svalue,'a) token
val RAT: (string) *  'a * 'a -> (svalue,'a) token
val BIGINT: (string) *  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val DIV_RAT:  'a * 'a -> (svalue,'a) token
val TIMES_RAT:  'a * 'a -> (svalue,'a) token
val MINUS_RAT:  'a * 'a -> (svalue,'a) token
val PLUS_RAT:  'a * 'a -> (svalue,'a) token
val MOD_INT:  'a * 'a -> (svalue,'a) token
val DIV_INT:  'a * 'a -> (svalue,'a) token
val TIMES_INT:  'a * 'a -> (svalue,'a) token
val MINUS_INT:  'a * 'a -> (svalue,'a) token
val PLUS_INT:  'a * 'a -> (svalue,'a) token
val TO_DECI:  'a * 'a -> (svalue,'a) token
val FROM_DECI:  'a * 'a -> (svalue,'a) token
val SHOW_DECI:  'a * 'a -> (svalue,'a) token
val SHOW_RAT:  'a * 'a -> (svalue,'a) token
val RATI:  'a * 'a -> (svalue,'a) token
val MAKE_RAT:  'a * 'a -> (svalue,'a) token
val INVERSE:  'a * 'a -> (svalue,'a) token
val GT_EQ:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val NOT_EQ:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val LT_EQ:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val RATIONAL:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val INTEGER:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
end
signature My_LRVALS=
sig
structure Tokens : My_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
