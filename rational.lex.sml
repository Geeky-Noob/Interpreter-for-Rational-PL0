functor MyLexFun(structure Tokens: My_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT | INITIAL
    structure UserDeclarations = 
      struct

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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\^@",#"\t",2),
(#"\v",#")",2),
(#"+",#"\255",2),
(#"\n",#"\n",3),
(#"*",#"*",4)], []), ([(#"\t",#"\t",6),
(#" ",#" ",6),
(#"\n",#"\n",7),
(#"!",#"!",8),
(#"%",#"%",9),
(#"&",#"&",10),
(#"(",#"(",11),
(#")",#")",12),
(#"*",#"*",13),
(#"+",#"+",14),
(#",",#",",15),
(#"-",#"-",16),
(#".",#".",17),
(#"/",#"/",18),
(#"0",#"9",19),
(#":",#":",20),
(#";",#";",21),
(#"<",#"<",22),
(#"=",#"=",23),
(#">",#">",24),
(#"A",#"Z",25),
(#"a",#"a",25),
(#"g",#"h",25),
(#"j",#"l",25),
(#"n",#"n",25),
(#"q",#"q",25),
(#"u",#"v",25),
(#"x",#"z",25),
(#"b",#"b",26),
(#"c",#"c",27),
(#"d",#"d",28),
(#"e",#"e",29),
(#"f",#"f",30),
(#"i",#"i",31),
(#"m",#"m",32),
(#"o",#"o",33),
(#"p",#"p",34),
(#"r",#"r",35),
(#"s",#"s",36),
(#"t",#"t",37),
(#"w",#"w",38),
(#"{",#"{",39),
(#"|",#"|",40),
(#"}",#"}",41),
(#"~",#"~",42)], []), ([], [56]), ([], [57]), ([(#")",#")",5)], [56]), ([], [3]), ([(#"\t",#"\t",6),
(#" ",#" ",6)], [1]), ([], [0]), ([], [10]), ([], [18]), ([(#"&",#"&",157)], []), ([(#"*",#"*",156)], [6]), ([], [7]), ([], [16]), ([(#".",#".",43),
(#"0",#"9",155)], [14]), ([], [13]), ([], [15]), ([(#"(",#"(",44),
(#"*",#"*",147),
(#"+",#"+",148),
(#"-",#"-",149),
(#"/",#"/",150),
(#"0",#"9",43)], []), ([], [17]), ([(#".",#".",43),
(#"0",#"9",19)], [54]), ([(#"=",#"=",146)], []), ([], [11]), ([(#"=",#"=",144),
(#">",#">",145)], [50]), ([], [51]), ([(#"=",#"=",143)], [49]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"n",25),
(#"p",#"z",25),
(#"o",#"o",137)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",134)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"n",25),
(#"p",#"z",25),
(#"o",#"o",133)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",130)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"e",25),
(#"g",#"h",25),
(#"j",#"q",25),
(#"s",#"z",25),
(#"f",#"f",118),
(#"i",#"i",119),
(#"r",#"r",120)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"e",25),
(#"g",#"m",25),
(#"o",#"z",25),
(#"f",#"f",106),
(#"n",#"n",107)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",99)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"c",25),
(#"e",#"z",25),
(#"d",#"d",98)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"q",25),
(#"s",#"z",25),
(#"r",#"r",87)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"d",25),
(#"f",#"z",25),
(#"a",#"a",77),
(#"e",#"e",78)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"g",25),
(#"i",#"z",25),
(#"h",#"h",64)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"g",25),
(#"i",#"n",25),
(#"p",#"s",25),
(#"u",#"z",25),
(#"h",#"h",52),
(#"o",#"o",53),
(#"t",#"t",54)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"g",25),
(#"i",#"z",25),
(#"h",#"h",48)], [53]), ([], [9]), ([(#"|",#"|",47)], []), ([], [8]), ([(#".",#".",43),
(#"0",#"9",19)], [19]), ([(#"(",#"(",44),
(#"0",#"9",43)], []), ([(#"0",#"9",45)], []), ([(#")",#")",46),
(#"0",#"9",45)], []), ([], [55]), ([], [26]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"h",25),
(#"j",#"z",25),
(#"i",#"i",49)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",50)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",51)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [35, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",62)], [53]), ([(#"0",#"9",25),
(#"A",#"C",25),
(#"E",#"Z",25),
(#"a",#"z",25),
(#"D",#"D",55)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [4, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",56)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"b",25),
(#"d",#"z",25),
(#"c",#"c",57)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"h",25),
(#"j",#"z",25),
(#"i",#"i",58)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"l",25),
(#"n",#"z",25),
(#"m",#"m",59)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",60)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",61)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [46, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"m",25),
(#"o",#"z",25),
(#"n",#"n",63)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [32, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"n",25),
(#"p",#"z",25),
(#"o",#"o",65)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"v",25),
(#"x",#"z",25),
(#"w",#"w",66)], [53]), ([(#"0",#"9",25),
(#"A",#"C",25),
(#"E",#"Q",25),
(#"S",#"Z",25),
(#"a",#"z",25),
(#"D",#"D",67),
(#"R",#"R",68)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",71)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",69)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"s",25),
(#"u",#"z",25),
(#"t",#"t",70)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [43, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"b",25),
(#"d",#"z",25),
(#"c",#"c",72)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"h",25),
(#"j",#"z",25),
(#"i",#"i",73)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"l",25),
(#"n",#"z",25),
(#"m",#"m",74)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",75)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",76)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [44, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"s",25),
(#"u",#"z",25),
(#"t",#"t",81)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",79)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"c",25),
(#"e",#"z",25),
(#"d",#"d",80)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [27, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"h",25),
(#"j",#"z",25),
(#"i",#"i",82)], [42, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"n",25),
(#"p",#"z",25),
(#"o",#"o",83)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"m",25),
(#"o",#"z",25),
(#"n",#"n",84)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",85)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",86)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [40, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"h",25),
(#"j",#"n",25),
(#"p",#"z",25),
(#"i",#"i",88),
(#"o",#"o",89)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"m",25),
(#"o",#"z",25),
(#"n",#"n",96)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"b",25),
(#"d",#"z",25),
(#"c",#"c",90)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",91)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"c",25),
(#"e",#"z",25),
(#"d",#"d",92)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"t",25),
(#"v",#"z",25),
(#"u",#"u",93)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"q",25),
(#"s",#"z",25),
(#"r",#"r",94)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",95)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [28, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"s",25),
(#"u",#"z",25),
(#"t",#"t",97)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [30, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [37, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"j",25),
(#"l",#"z",25),
(#"k",#"k",100)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",101)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25),
(#"_",#"_",102)], [53]), ([(#"r",#"r",103)], []), ([(#"a",#"a",104)], []), ([(#"t",#"t",105)], []), ([], [41]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [31, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"s",25),
(#"u",#"u",25),
(#"w",#"z",25),
(#"t",#"t",108),
(#"v",#"v",109)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",114)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",110)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"q",25),
(#"s",#"z",25),
(#"r",#"r",111)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"r",25),
(#"t",#"z",25),
(#"s",#"s",112)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",113)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [24, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"f",25),
(#"h",#"z",25),
(#"g",#"g",115)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",116)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"q",25),
(#"s",#"z",25),
(#"r",#"r",117)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [38, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [5, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [34, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"n",25),
(#"p",#"z",25),
(#"o",#"o",121)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"l",25),
(#"n",#"z",25),
(#"m",#"m",122)], [53]), ([(#"0",#"9",25),
(#"A",#"C",25),
(#"E",#"Z",25),
(#"a",#"z",25),
(#"D",#"D",123)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",124)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"b",25),
(#"d",#"z",25),
(#"c",#"c",125)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"h",25),
(#"j",#"z",25),
(#"i",#"i",126)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"l",25),
(#"n",#"z",25),
(#"m",#"m",127)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",128)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",129)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [45, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"r",25),
(#"t",#"z",25),
(#"s",#"s",131)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",132)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [33, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [36, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",135)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",136)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [29, 53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"n",25),
(#"p",#"z",25),
(#"o",#"o",138)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"k",25),
(#"m",#"z",25),
(#"l",#"l",139)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"d",25),
(#"f",#"z",25),
(#"e",#"e",140)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"b",#"z",25),
(#"a",#"a",141)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"m",25),
(#"o",#"z",25),
(#"n",#"n",142)], [53]), ([(#"0",#"9",25),
(#"A",#"Z",25),
(#"a",#"z",25)], [39, 53]), ([], [48]), ([], [47]), ([], [52]), ([], [12]), ([(#".",#".",154)], []), ([(#".",#".",153)], []), ([(#".",#".",152)], []), ([(#".",#".",151)], []), ([], [23]), ([], [21]), ([], [20]), ([], [22]), ([(#".",#".",43),
(#"0",#"9",155)], []), ([], [2]), ([], [25])]
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( pos:= (!pos) + 1; lex()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT;lex()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;lex()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TRUE(!pos, !pos )))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FALSE(!pos, !pos )))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(!pos, !pos )))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(!pos, !pos )))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(!pos, !pos )))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(!pos, !pos )))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOT(!pos, !pos )))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(!pos, !pos )))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(!pos, !pos )))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(!pos, !pos )))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS_INT(!pos, !pos )))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS_INT(!pos, !pos )))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES_INT(!pos, !pos )))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIV_INT(!pos, !pos )))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MOD_INT(!pos, !pos )))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEG(!pos, !pos )))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS_RAT(!pos, !pos )))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS_RAT(!pos, !pos )))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES_RAT(!pos, !pos )))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIV_RAT(!pos, !pos )))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INVERSE(!pos, !pos )))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( Tokens.AND(!pos, !pos )))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(!pos, !pos )))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.READ(!pos, !pos)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PROCEDURE(!pos, !pos)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CALL(!pos, !pos)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PRINT(!pos, !pos)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(!pos, !pos )))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN(!pos, !pos )))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE(!pos, !pos )))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FI(!pos, !pos )))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE(!pos, !pos )))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO(!pos, !pos )))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OD(!pos, !pos )))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTEGER(!pos, !pos )))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOL(!pos, !pos )))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATIONAL(!pos, !pos )))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MAKE_RAT(!pos, !pos )))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATI(!pos, !pos )))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOW_RAT(!pos, !pos )))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOW_DECI(!pos, !pos )))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FROM_DECI(!pos, !pos )))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TO_DECI(!pos, !pos)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT_EQ(!pos, !pos )))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT_EQ(!pos, !pos )))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(!pos, !pos )))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(!pos, !pos )))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(!pos, !pos )))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOT_EQ(!pos, !pos )))
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID( yytext ,!pos ,!pos ))
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BIGINT( yytext ,!pos ,!pos))
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RAT( yytext ,!pos ,!pos))
      end
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of COMMENT => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
