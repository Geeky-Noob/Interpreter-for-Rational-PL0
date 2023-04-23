structure DataTypes = 
struct
datatype AST  = PROG of BLK
and BLK = BLK of (DEC)*(CMD list)
and DEC = DEC of (VARDEC list)*(PRODEC list)
and VARDEC=VARDEC of (string list)*Type 
and PRODEC = PRODEC of (string)*(BLK)
and Type = BigInt | Rational | Bool|String
and CMD = SET of string*Exp | WH of Exp*(CMD list)| ITE of Exp*(CMD list)*(CMD list) | Read of string| Call of string| Print of Type*Exp
and Exp =  NOT of Exp
          | AND of Exp*Exp
          | OR of Exp*Exp
          | PLUS_INT of Exp*Exp
          | MINUS_INT of Exp*Exp
          | TIMES_INT of Exp*Exp
          | MOD_INT of Exp * Exp 
          | DIV_INT of Exp * Exp
          | PLUS_RAT of Exp*Exp
          | MINUS_RAT of Exp*Exp
          | TIMES_RAT of Exp*Exp
          | DIV_RAT of Exp * Exp  
          | LT of Exp*Exp
          | LT_EQ of Exp*Exp
          | NOT_EQ of Exp*Exp
          | NEG of Exp
          | EQ of Exp*Exp
          | GT of Exp*Exp
          | GT_EQ of Exp * Exp 
          | INVERSE of Exp
          | MAKE_RAT of Exp*Exp
          | RATI of Exp
          | SHOW_RAT of Exp
          | SHOW_DECI of Exp
          | FROM_DECI of Exp
          | TO_DECI of Exp
          | TT
          | FF 
          | Big_int of string
          | Rat of string 
          | Ident of string 
end ; 
