# COL226 Assignment-4 : The Imperative Language Rational-PL0

## By Ojasvi Bansal (2021CS50600)

Grammar (in EBNF form) used for scanning and parsing of a program written in Rational-PL0 is as follows:-

    Program ::= Block .
    Block ::= DeclarationSeq CommandSeq .
    DeclarationSeq ::= [VarDecls] [ProcDecls] .
    VarDecls ::= [RatVarDecls]| [IntVarDecls]| [BoolVarDecls] .
    RatVarDecls ::= rational Ident {, Ident}; .
    IntVarDecls ::= integer Ident {, Ident}; .
    BoolVarDecls ::= boolean Ident {, Ident}; .
    ProcDecls ::= [ProcDef {;ProcDecls};] .
    ProcDef ::= procedure Ident Block .
    CommandSeq ::= {{Command;}} .
    Command ::= AssignmentCmd| CallCmd| ReadCmd |PrintCmd |ConditionalCmd | WhileCmd.
    AssignmentCmd ::= Ident := Expression .
    CallCmd ::= call Ident .
    ReadCmd ::= read (Ident) .
    PrintCmd ::= print (Expression) .
    Expression ::= RatExpression | IntExpression | BoolExpression .
    ConditionalCmd ::= if BoolExpression then CommandSeq else CommandSeq fi .
    WhileCmd ::= while BoolExpression do CommandSeq od .
    RatExp2 ::= RatExpression | RatExp2 ".*." RatExpression | RatExp2 "./." RatExpression.
    RatExpression ::= rational | Ident | "(" RatExp1 ")" | make_rat "(" IntExpression , IntExpression ")" | fromDecimal "(" RatExpression ")" | inverse "(" RatExpression ")"| ~ RatExpression .
    IntExp1 ::= IntExp2 | IntExp1 "+" IntExp2 | IntExp1 "-" IntExp2.
    IntExp2 ::= IntExpression| IntExp2 "*" IntExpression | IntExp2 "/" IntExpression| IntExp2 "%" IntExpression.
    IntExpression ::= integer | Ident | "(" IntExp1 ")"| ~ IntExpression.
    BoolExp1 ::= BoolExp2 | BoolExp1 "||" BoolExp2.
    BoolExp2 ::= BoolExpression| BoolExp2 "&&" BoolExpression.
    BoolExpression ::= boolean | Ident | "(" BoolExp1 ")"| ! BoolExpression | RatExpression "<" RatExpression | RatExpression ">" RatExpresssion | RatExpression "=" RatExpression | RatExpression ">=" RatExpression | RatExpression "<=" RatExpression | RatExpression "<>" RatExpression | IntExpression "<" IntExpression | IntExpression ">" IntExpresssion | IntExpression "=" IntExpression | IntExpression ">=" IntExpression | IntExpression "<=" IntExpression | IntExpression "<>" IntExpression.
    RatExp1 ::= RatExp2 | RatExp1 ".+." RatExp2 | RatExp1 ".-." RatExp2.
    rational ::= decimal.
    integer ::= digit | integer digit.
    decimal ::= sign d "." d "(" integer ")".
    d ::= integer | "".
    sign ::= "+" | "~"| "".
    dig ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".
    boolean ::= tt | ff
    Ident ::= letter{letter|"_"|dig}.
    letter ::= upcase | lowcase.
    upcase ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z".
    lowcase ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z".
