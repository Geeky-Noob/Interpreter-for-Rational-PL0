functor MyLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : My_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes ; 

exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ; 


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\018\000\000\000\
\\001\000\002\000\034\000\000\000\
\\001\000\003\000\037\000\000\000\
\\001\000\003\000\038\000\000\000\
\\001\000\003\000\054\000\018\000\053\000\025\000\052\000\026\000\051\000\
\\027\000\050\000\028\000\049\000\029\000\048\000\030\000\047\000\
\\031\000\046\000\041\000\045\000\044\000\044\000\045\000\043\000\
\\051\000\042\000\052\000\041\000\053\000\040\000\000\000\
\\001\000\003\000\080\000\000\000\
\\001\000\003\000\081\000\000\000\
\\001\000\003\000\082\000\000\000\
\\001\000\003\000\083\000\000\000\
\\001\000\003\000\084\000\000\000\
\\001\000\003\000\085\000\000\000\
\\001\000\004\000\090\000\000\000\
\\001\000\004\000\091\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\004\000\116\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\004\000\119\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\004\000\120\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\004\000\121\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\004\000\122\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\004\000\123\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\004\000\128\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\005\000\000\000\000\000\
\\001\000\007\000\125\000\000\000\
\\001\000\008\000\089\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\009\000\129\000\000\000\
\\001\000\011\000\078\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\012\000\118\000\000\000\
\\001\000\013\000\019\000\000\000\
\\001\000\013\000\033\000\000\000\
\\001\000\013\000\056\000\000\000\
\\001\000\014\000\124\000\019\000\077\000\020\000\076\000\021\000\075\000\
\\022\000\074\000\023\000\073\000\024\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\
\\037\000\066\000\038\000\065\000\039\000\064\000\040\000\063\000\
\\042\000\062\000\043\000\061\000\000\000\
\\001\000\048\000\035\000\000\000\
\\001\000\053\000\012\000\000\000\
\\001\000\053\000\022\000\000\000\
\\001\000\053\000\036\000\000\000\
\\001\000\053\000\059\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\006\000\030\000\010\000\029\000\047\000\028\000\049\000\027\000\
\\050\000\026\000\053\000\025\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\015\000\010\000\016\000\009\000\017\000\008\000\000\000\
\\138\000\000\000\
\\139\000\046\000\016\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\014\000\020\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\019\000\077\000\020\000\076\000\021\000\075\000\022\000\074\000\
\\023\000\073\000\024\000\072\000\032\000\071\000\033\000\070\000\
\\034\000\069\000\035\000\068\000\036\000\067\000\037\000\066\000\
\\038\000\065\000\039\000\064\000\040\000\063\000\042\000\062\000\
\\043\000\061\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\032\000\071\000\033\000\070\000\034\000\069\000\035\000\068\000\
\\036\000\067\000\037\000\066\000\038\000\065\000\039\000\064\000\
\\040\000\063\000\000\000\
\\155\000\032\000\071\000\033\000\070\000\034\000\069\000\035\000\068\000\
\\036\000\067\000\037\000\066\000\038\000\065\000\039\000\064\000\
\\040\000\063\000\000\000\
\\156\000\019\000\077\000\020\000\076\000\023\000\073\000\024\000\072\000\
\\032\000\071\000\033\000\070\000\034\000\069\000\035\000\068\000\
\\036\000\067\000\037\000\066\000\038\000\065\000\039\000\064\000\
\\040\000\063\000\000\000\
\\157\000\019\000\077\000\020\000\076\000\023\000\073\000\024\000\072\000\
\\032\000\071\000\033\000\070\000\034\000\069\000\035\000\068\000\
\\036\000\067\000\037\000\066\000\038\000\065\000\039\000\064\000\
\\040\000\063\000\000\000\
\\158\000\032\000\071\000\033\000\070\000\034\000\069\000\035\000\068\000\
\\036\000\067\000\037\000\066\000\038\000\065\000\039\000\064\000\
\\040\000\063\000\000\000\
\\159\000\032\000\071\000\033\000\070\000\034\000\069\000\035\000\068\000\
\\036\000\067\000\037\000\066\000\038\000\065\000\039\000\064\000\
\\040\000\063\000\000\000\
\\160\000\034\000\069\000\035\000\068\000\036\000\067\000\039\000\064\000\
\\040\000\063\000\000\000\
\\161\000\034\000\069\000\035\000\068\000\036\000\067\000\039\000\064\000\
\\040\000\063\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\034\000\069\000\035\000\068\000\036\000\067\000\039\000\064\000\
\\040\000\063\000\000\000\
\\166\000\034\000\069\000\035\000\068\000\036\000\067\000\039\000\064\000\
\\040\000\063\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\019\000\077\000\020\000\076\000\021\000\075\000\022\000\074\000\
\\023\000\073\000\024\000\072\000\032\000\071\000\033\000\070\000\
\\034\000\069\000\035\000\068\000\036\000\067\000\037\000\066\000\
\\038\000\065\000\039\000\064\000\040\000\063\000\000\000\
\\177\000\019\000\077\000\020\000\076\000\021\000\075\000\022\000\074\000\
\\023\000\073\000\024\000\072\000\032\000\071\000\033\000\070\000\
\\034\000\069\000\035\000\068\000\036\000\067\000\037\000\066\000\
\\038\000\065\000\039\000\064\000\040\000\063\000\042\000\062\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\"
val actionRowNumbers =
"\041\000\031\000\041\000\043\000\
\\000\000\035\000\048\000\049\000\
\\047\000\026\000\050\000\042\000\
\\043\000\040\000\032\000\036\000\
\\038\000\045\000\031\000\044\000\
\\041\000\027\000\001\000\030\000\
\\033\000\002\000\003\000\004\000\
\\004\000\051\000\028\000\038\000\
\\037\000\004\000\054\000\034\000\
\\004\000\024\000\085\000\087\000\
\\086\000\084\000\083\000\004\000\
\\005\000\006\000\007\000\008\000\
\\009\000\010\000\004\000\004\000\
\\004\000\022\000\046\000\039\000\
\\055\000\011\000\012\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\000\000\088\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\073\000\089\000\013\000\000\000\
\\052\000\053\000\081\000\080\000\
\\072\000\071\000\070\000\069\000\
\\068\000\067\000\066\000\065\000\
\\064\000\063\000\062\000\060\000\
\\061\000\059\000\058\000\025\000\
\\014\000\015\000\016\000\017\000\
\\018\000\029\000\082\000\021\000\
\\057\000\079\000\078\000\077\000\
\\076\000\075\000\004\000\000\000\
\\019\000\023\000\074\000\056\000\
\\020\000"
val gotoT =
"\
\\001\000\128\000\002\000\005\000\005\000\004\000\006\000\003\000\
\\007\000\002\000\011\000\001\000\000\000\
\\012\000\009\000\000\000\
\\006\000\011\000\007\000\002\000\011\000\001\000\000\000\
\\008\000\013\000\009\000\012\000\000\000\
\\003\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\019\000\009\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\022\000\010\000\021\000\000\000\
\\000\000\
\\012\000\029\000\000\000\
\\000\000\
\\002\000\030\000\005\000\004\000\006\000\003\000\007\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\037\000\000\000\
\\013\000\053\000\000\000\
\\000\000\
\\000\000\
\\004\000\055\000\010\000\021\000\000\000\
\\000\000\
\\013\000\056\000\000\000\
\\000\000\
\\000\000\
\\013\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\084\000\000\000\
\\013\000\085\000\000\000\
\\013\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\090\000\000\000\
\\013\000\091\000\000\000\
\\013\000\092\000\000\000\
\\013\000\093\000\000\000\
\\013\000\094\000\000\000\
\\013\000\095\000\000\000\
\\013\000\096\000\000\000\
\\013\000\097\000\000\000\
\\013\000\098\000\000\000\
\\013\000\099\000\000\000\
\\013\000\100\000\000\000\
\\013\000\101\000\000\000\
\\013\000\102\000\000\000\
\\013\000\103\000\000\000\
\\013\000\104\000\000\000\
\\013\000\105\000\000\000\
\\013\000\106\000\000\000\
\\003\000\107\000\000\000\
\\000\000\
\\013\000\108\000\000\000\
\\013\000\109\000\000\000\
\\013\000\110\000\000\000\
\\013\000\111\000\000\000\
\\013\000\112\000\000\000\
\\013\000\113\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\115\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\124\000\000\000\
\\003\000\125\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 129
val numrules = 55
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | RAT of unit ->  (string)
 | BIGINT of unit ->  (string) | expression of unit ->  (Type*Exp)
 | varlist of unit ->  (string list) | typedec of unit ->  (Type)
 | command of unit ->  (CMD) | prodec of unit ->  (PRODEC)
 | prodecseq of unit ->  (PRODEC list) | vardec of unit ->  (VARDEC)
 | vardecseq of unit ->  (VARDEC list) | dec of unit ->  (DEC)
 | commands of unit ->  (CMD list) | comseq of unit ->  (CMD list)
 | block of unit ->  (BLK) | start of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 4) => true | _ => false
val showTerminal =
fn (T 0) => "LBRACE"
  | (T 1) => "RBRACE"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "EOF"
  | (T 5) => "IF"
  | (T 6) => "ELSE"
  | (T 7) => "THEN"
  | (T 8) => "FI"
  | (T 9) => "WHILE"
  | (T 10) => "DO"
  | (T 11) => "OD"
  | (T 12) => "SEMICOLON"
  | (T 13) => "COMMA"
  | (T 14) => "INTEGER"
  | (T 15) => "BOOL"
  | (T 16) => "RATIONAL"
  | (T 17) => "NEG"
  | (T 18) => "LT_EQ"
  | (T 19) => "LT"
  | (T 20) => "NOT_EQ"
  | (T 21) => "EQ"
  | (T 22) => "GT"
  | (T 23) => "GT_EQ"
  | (T 24) => "INVERSE"
  | (T 25) => "MAKE_RAT"
  | (T 26) => "RATI"
  | (T 27) => "SHOW_RAT"
  | (T 28) => "SHOW_DECI"
  | (T 29) => "FROM_DECI"
  | (T 30) => "TO_DECI"
  | (T 31) => "PLUS_INT"
  | (T 32) => "MINUS_INT"
  | (T 33) => "TIMES_INT"
  | (T 34) => "DIV_INT"
  | (T 35) => "MOD_INT"
  | (T 36) => "PLUS_RAT"
  | (T 37) => "MINUS_RAT"
  | (T 38) => "TIMES_RAT"
  | (T 39) => "DIV_RAT"
  | (T 40) => "NOT"
  | (T 41) => "AND"
  | (T 42) => "OR"
  | (T 43) => "TRUE"
  | (T 44) => "FALSE"
  | (T 45) => "PROCEDURE"
  | (T 46) => "PRINT"
  | (T 47) => "ASSIGN"
  | (T 48) => "READ"
  | (T 49) => "CALL"
  | (T 50) => "BIGINT"
  | (T 51) => "RAT"
  | (T 52) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43)
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 
0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, block1left, block1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (block
 as block1) = block1 ()
 in (PROG(block))
end)
 in ( LrTable.NT 0, ( result, block1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.comseq comseq1, _, comseq1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.block (fn _ => let val  (dec as dec1) = dec1 ()
 val  (comseq as comseq1) = comseq1 ()
 in (BLK(dec, comseq))
end)
 in ( LrTable.NT 1, ( result, dec1left, comseq1right), rest671)
end
|  ( 2, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.commands 
commands1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.comseq (fn _ => let val  (commands as 
commands1) = commands1 ()
 in ((commands))
end)
 in ( LrTable.NT 2, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.commands (fn _ => (
([])))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.commands commands1, _, commands1right)) :: _
 :: ( _, ( MlyValue.command command1, command1left, _)) :: rest671))
 => let val  result = MlyValue.commands (fn _ => let val  (command as 
command1) = command1 ()
 val  (commands as commands1) = commands1 ()
 in ((command::commands))
end)
 in ( LrTable.NT 3, ( result, command1left, commands1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.prodecseq prodecseq1, _, prodecseq1right))
 :: ( _, ( MlyValue.vardecseq vardecseq1, vardecseq1left, _)) :: 
rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
vardecseq as vardecseq1) = vardecseq1 ()
 val  (prodecseq as prodecseq1) = prodecseq1 ()
 in ((DEC(vardecseq,prodecseq)))
end)
 in ( LrTable.NT 4, ( result, vardecseq1left, prodecseq1right), 
rest671)
end
|  ( 6, ( rest671)) => let val  result = MlyValue.vardecseq (fn _ => (
([])))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( MlyValue.vardecseq vardecseq1, _, vardecseq1right))
 :: ( _, ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) =>
 let val  result = MlyValue.vardecseq (fn _ => let val  (vardec as 
vardec1) = vardec1 ()
 val  (vardecseq as vardecseq1) = vardecseq1 ()
 in ((vardec::vardecseq))
end)
 in ( LrTable.NT 5, ( result, vardec1left, vardecseq1right), rest671)

end
|  ( 8, ( rest671)) => let val  result = MlyValue.prodecseq (fn _ => (
([])))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( MlyValue.prodecseq prodecseq1, _, prodecseq1right))
 :: ( _, ( MlyValue.prodec prodec1, prodec1left, _)) :: rest671)) =>
 let val  result = MlyValue.prodecseq (fn _ => let val  (prodec as 
prodec1) = prodec1 ()
 val  (prodecseq as prodecseq1) = prodecseq1 ()
 in ((prodec::prodecseq))
end)
 in ( LrTable.NT 7, ( result, prodec1left, prodecseq1right), rest671)

end
|  ( 10, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( MlyValue.typedec typedec1, typedec1left, _)
) :: rest671)) => let val  result = MlyValue.vardec (fn _ => let val 
 (typedec as typedec1) = typedec1 ()
 val  (varlist as varlist1) = varlist1 ()
 in (VARDEC(varlist , typedec))
end)
 in ( LrTable.NT 6, ( result, typedec1left, SEMICOLON1right), rest671)

end
|  ( 11, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.block 
block1, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
PROCEDURE1left, _)) :: rest671)) => let val  result = MlyValue.prodec
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (block as block1) = block1 ()
 in (PRODEC(ID, block))
end)
 in ( LrTable.NT 8, ( result, PROCEDURE1left, SEMICOLON1right), 
rest671)
end
|  ( 12, ( ( _, ( _, INTEGER1left, INTEGER1right)) :: rest671)) => let
 val  result = MlyValue.typedec (fn _ => ((BigInt)))
 in ( LrTable.NT 10, ( result, INTEGER1left, INTEGER1right), rest671)

end
|  ( 13, ( ( _, ( _, RATIONAL1left, RATIONAL1right)) :: rest671)) =>
 let val  result = MlyValue.typedec (fn _ => ((Rational)))
 in ( LrTable.NT 10, ( result, RATIONAL1left, RATIONAL1right), rest671
)
end
|  ( 14, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.typedec (fn _ => ((Bool)))
 in ( LrTable.NT 10, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.varlist (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (([ID]))
end)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _
 :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.varlist (fn _ => let val  (ID as ID1) = ID1 ()
 val  (varlist as varlist1) = varlist1 ()
 in ((ID::varlist))
end)
 in ( LrTable.NT 11, ( result, ID1left, varlist1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ID ID1, _, _
)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (ID as ID1) = ID1 ()
 in ((Read(ID)))
end)
 in ( LrTable.NT 9, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.command (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (Print(expression))
end)
 in ( LrTable.NT 9, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
CALL1left, _)) :: rest671)) => let val  result = MlyValue.command (fn
 _ => let val  (ID as ID1) = ID1 ()
 in ((Call(ID)))
end)
 in ( LrTable.NT 9, ( result, CALL1left, ID1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let
 val  result = MlyValue.command (fn _ => let val  (ID as ID1) = ID1 ()
 val  (expression as expression1) = expression1 ()
 in ((SET(ID, #2 expression)))
end)
 in ( LrTable.NT 9, ( result, ID1left, expression1right), rest671)
end
|  ( 21, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.comseq comseq2,
 _, _)) :: _ :: ( _, ( MlyValue.comseq comseq1, _, _)) :: _ :: ( _, ( 
MlyValue.expression expression1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.command (fn _ => let val  (
expression as expression1) = expression1 ()
 val  comseq1 = comseq1 ()
 val  comseq2 = comseq2 ()
 in ((ITE(#2 expression, comseq1, comseq2)))
end)
 in ( LrTable.NT 9, ( result, IF1left, FI1right), rest671)
end
|  ( 22, ( ( _, ( _, _, OD1right)) :: ( _, ( MlyValue.comseq comseq1,
 _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _)) :: ( _,
 ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  (comseq as comseq1) = comseq1 ()
 in ((WH( #2 expression , comseq )))
end)
 in ( LrTable.NT 9, ( result, WHILE1left, OD1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool , LT_EQ(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool , LT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool , EQ(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool , NOT_EQ(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool , GT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool , GT_EQ(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((BigInt , PLUS_INT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((BigInt , MINUS_INT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((BigInt , TIMES_INT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((BigInt , DIV_INT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((BigInt , MOD_INT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Rational , PLUS_RAT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Rational , MINUS_RAT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Rational , TIMES_RAT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Rational , DIV_RAT(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 38, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, INVERSE1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ((Rational , INVERSE(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, INVERSE1left, expression1right), 
rest671)
end
|  ( 39, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression2, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _,
 _)) :: _ :: ( _, ( _, MAKE_RAT1left, _)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => let val  expression1 = 
expression1 ()
 val  expression2 = expression2 ()
 in ((Rational , MAKE_RAT(#2 expression1, #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, MAKE_RAT1left, RPAREN1right), rest671)

end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, RATI1left, _)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  (expression
 as expression1) = expression1 ()
 in ((Rational , RATI(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, RATI1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, SHOW_RAT1left, _)) :: rest671))
 => let val  result = MlyValue.expression (fn _ => let val  (
expression as expression1) = expression1 ()
 in ((Rational , SHOW_RAT(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, SHOW_RAT1left, RPAREN1right), rest671)

end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, SHOW_DECI1left, _)) :: rest671))
 => let val  result = MlyValue.expression (fn _ => let val  (
expression as expression1) = expression1 ()
 in ((Rational , SHOW_DECI(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, SHOW_DECI1left, RPAREN1right), rest671)

end
|  ( 43, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, FROM_DECI1left, _)) :: rest671))
 => let val  result = MlyValue.expression (fn _ => let val  (
expression as expression1) = expression1 ()
 in ((Rational , FROM_DECI(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, FROM_DECI1left, RPAREN1right), rest671)

end
|  ( 44, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, TO_DECI1left, _)) :: rest671))
 => let val  result = MlyValue.expression (fn _ => let val  (
expression as expression1) = expression1 ()
 in ((Rational , TO_DECI(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, TO_DECI1left, RPAREN1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool, AND(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 46, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((Bool, OR(#2 expression1 , #2 expression2)))
end)
 in ( LrTable.NT 12, ( result, expression1left, expression2right), 
rest671)
end
|  ( 47, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in ((expression))
end)
 in ( LrTable.NT 12, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 48, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => ((Bool , TT )))
 in ( LrTable.NT 12, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 49, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => ((Bool , FF )))
 in ( LrTable.NT 12, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  (ID as ID1) =
 ID1 ()
 in ((String, Ident(ID)))
end)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.BIGINT BIGINT1, BIGINT1left, BIGINT1right))
 :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  (BIGINT as BIGINT1) = BIGINT1 ()
 in ((BigInt , Big_int(BIGINT) ))
end)
 in ( LrTable.NT 12, ( result, BIGINT1left, BIGINT1right), rest671)

end
|  ( 52, ( ( _, ( MlyValue.RAT RAT1, RAT1left, RAT1right)) :: rest671)
) => let val  result = MlyValue.expression (fn _ => let val  (RAT as 
RAT1) = RAT1 ()
 in ((Rational, Rat(RAT)))
end)
 in ( LrTable.NT 12, ( result, RAT1left, RAT1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ((Bool, NOT(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, NOT1left, expression1right), rest671)

end
|  ( 54, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ((#1 expression, NEG(#2 expression)))
end)
 in ( LrTable.NT 12, ( result, NEG1left, expression1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : My_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LT_EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT_EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT_EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKE_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun RATI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOW_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOW_DECI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun FROM_DECI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun TO_DECI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun BIGINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.BIGINT (fn () => i),p1,p2))
fun RAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.RAT (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
end
end
