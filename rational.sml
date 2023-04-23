structure My :
sig val interpret : string * string -> unit
end =
    struct
    exception SyntaxError;
    exception VariableRedeclarationException of string ; 
    exception ProcedureNotDeclared;
    exception TypeMisMatchException ; 
    exception UnDeclaredVariableException ; 
    fun interpret(inputfile,outputfile)=
        let
            val ins = TextIO.openIn inputfile;
            val grab : int -> string = fn n => if TextIO.endOfStream ins then ""
                                            else TextIO.inputN (ins,n);
            val printError : string * int * int -> unit = fn (msg,line,col) => print (inputfile^"["^Int.toString line^":"^Int.toString col^"] "^msg^"\n");
            val (tree,rem) = MyParser.parse (15,(MyParser.makeLexer grab ),printError,()) handle MyParser.ParseError => raise SyntaxError ; 
            val _ = TextIO.closeIn ins;
            val outs = TextIO.openOut outputfile;
            val DataTypes.PROG(blk)=tree
            val DataTypes.BLK(dec, com_list)=blk

            (* val cur_scope=ref 0; *)
            val scope_id = ref 0;
            val parent_id = ref ~1;
            fun my_hash(parent_id, name)=
                    HashString.hashString(Int.toString(parent_id)^name)
            val varTable : (int * string, DataTypes.Type * int * string) HashTable.hash_table =
                HashTable.mkTable (my_hash, op=) (42, Fail "identifier not found") ;
            fun insertInVarTable( idList : string list , (idType : DataTypes.Type, parent_id : int, value : string)) = 
                    if (null idList) then () 
                    else ( if (isSome(HashTable.find varTable (parent_id,hd idList)) andalso #2 (valOf(HashTable.find varTable (parent_id, hd idList))) = parent_id andalso #1 (valOf(HashTable.find varTable (parent_id, hd idList))) = idType) then ()
                        else (HashTable.insert varTable ((parent_id, hd idList) , (idType, parent_id, value)) ); 
                            insertInVarTable( tl idList , (idType, parent_id, value)) ) ; 
            val procTable : (int * string, DataTypes.BLK * int * int) HashTable.hash_table =
                HashTable.mkTable (my_hash, op=) (42, Fail "procedure not declared") ;
            fun insertInProcTable( id , (block : DataTypes.BLK, scope_id : int, parent_id : int)) = 
                    if (isSome(HashTable.find procTable (parent_id,id)) andalso #3(valOf(HashTable.find procTable (parent_id,id))) = parent_id) then ()
                        else HashTable.insert procTable ( (parent_id,id) , (block, scope_id, parent_id) );

            fun find(i,[])=raise SyntaxError
            |   find(i:string, a::rest : (string*string)list)= if i= #1 a then #2 a else find(i, rest)
            fun isRational(s)=
                let
                    val l=explode(s)
                    fun check(a::[])= false
                    |   check(a::rest)=if a= #"(" then true else check(rest)
                in
                    check(l)
                end

            fun getVar(id,parent_id)= if Option.isSome(HashTable.find varTable (parent_id, id)) = true then (valOf(HashTable.find varTable (parent_id, id))) else if (parent_id > ~1) then (getVar(id,parent_id-1)) else raise UnDeclaredVariableException
            fun getProcedure(id,parent_id)= if Option.isSome(HashTable.find procTable (parent_id, id)) = true then (valOf(HashTable.find procTable (parent_id, id))) else if (parent_id > ~1) then (getProcedure(id,parent_id-1)) else raise ProcedureNotDeclared
            fun getId(id,parent_id)= if Option.isSome(HashTable.find varTable (parent_id, id)) = true then (#3 (valOf(HashTable.find varTable (parent_id, id)))) else if (parent_id > ~1) then (getId(id,parent_id-1)) else "Error"
            fun evaluateExp(exp)=
                case (exp) of 
                        DataTypes.Big_int i => (i)
                |       DataTypes.Rat i => (i)
                |       DataTypes.TT => ("tt")
                |       DataTypes.FF => ("ff")
                |       DataTypes.Ident i => (getId(i,!parent_id))
                |       DataTypes.PLUS_INT (exp1,exp2) =>(
                                let val v = BigInt.toString(BigInt.add(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in (v)
                                end)
                |       DataTypes.MINUS_INT (exp1,exp2) => 
                                let val v = BigInt.toString(BigInt.subtract(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in (v)
                                end
                |       DataTypes.TIMES_INT (exp1,exp2) => 
                                let val v = BigInt.toString(BigInt.multiply(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in (v)
                                end
                |       DataTypes.DIV_INT (exp1,exp2) => 
                                let val v = BigInt.toString(#1 (BigInt.divide(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2)))))
                                in (v)
                                end
                |       DataTypes.MOD_INT (exp1,exp2) => 
                                let val v = BigInt.toString(#2 (BigInt.divide(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2)))))
                                in (v)
                                end
                |       DataTypes.PLUS_RAT (exp1,exp2) => 
                                let val v = Rational.showDecimal(Rational.add(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (v)
                                end
                |       DataTypes.MINUS_RAT (exp1,exp2) => 
                                let val v = Rational.showDecimal(Rational.subtract(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (v)
                                end
                |       DataTypes.TIMES_RAT (exp1,exp2) => 
                                let val v = Rational.showDecimal(Rational.multiply(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (v)
                                end
                |       DataTypes.DIV_RAT (exp1,exp2) => 
                                let val v = Rational.showDecimal(valOf(Rational.divide(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2)))))
                                in (v)
                                end
                |       DataTypes.MAKE_RAT(exp1, exp2) => Rational.showDecimal(valOf(Rational.make_rat(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2)))))
                |       DataTypes.RATI(exp1) => Rational.showDecimal(valOf(Rational.rat(BigInt.fromString(evaluateExp(exp1)))))
                |       DataTypes.SHOW_RAT(exp) => Rational.showRat(Rational.fromDecimal(evaluateExp(exp)))
                |       DataTypes.SHOW_DECI(exp) => (evaluateExp(exp))
                |       DataTypes.FROM_DECI(exp) => Rational.showDecimal(Rational.fromDecimal(evaluateExp(exp)))
                |       DataTypes.TO_DECI(exp) => (evaluateExp(exp))
                |       DataTypes.LT (exp1, exp2) =>
                            if (isRational(evaluateExp(exp1))=false) then 
                                let 
                                        val v = (BigInt.compare(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in 
                                        if (v = LESS) then ("tt") else ("ff")
                                end
                            else 
                                let val v = (Rational.less(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (if v=true then "tt" else "ff")
                                end
                |       DataTypes.GT (exp1, exp2) =>
                            if (isRational(evaluateExp(exp1))=false) then 
                                let 
                                        val v = (BigInt.compare(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in 
                                        if (v = GREATER) then ("tt") else ("ff")
                                end
                            else
                                let val v = (Rational.less(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))) andalso Rational.equal(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (if v=true then "ff" else "tt")
                                end
                |       DataTypes.EQ (exp1, exp2) =>
                            if (isRational(evaluateExp(exp1))=false) then
                                let 
                                        val v = (BigInt.compare(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in 
                                        if (v = EQUAL) then ("tt") else ("ff")
                                end
                            else
                                let val v = (Rational.equal(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (if v=true then "tt" else "ff")
                                end
                |       DataTypes.NOT_EQ (exp1, exp2) =>
                            if (isRational(evaluateExp(exp1))=false) then
                                let 
                                        val v = (BigInt.compare(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in 
                                        if (v = EQUAL) then ("ff") else ("tt")
                                end
                            else
                                let val v = (Rational.equal(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (if v=true then "ff" else "tt")
                                end
                |       DataTypes.LT_EQ (exp1, exp2) =>
                            if (isRational(evaluateExp(exp1))=false) then
                                let 
                                        val v = (BigInt.compare(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in 
                                        if (v = GREATER) then ("ff") else ("tt")
                                end
                            else
                                let val v = (Rational.less(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2)))) orelse (Rational.equal(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (if v=true then "tt" else "ff")
                                end
                |       DataTypes.GT_EQ (exp1, exp2) =>
                            if (isRational(evaluateExp(exp1))=false) then
                                let 
                                        val v = (BigInt.compare(BigInt.fromString(evaluateExp(exp1)), BigInt.fromString(evaluateExp(exp2))))
                                in 
                                        if (v = LESS) then ("ff") else ("tt")
                                end
                            else
                                let val v = (Rational.less(Rational.fromDecimal(evaluateExp(exp1)), Rational.fromDecimal(evaluateExp(exp2))))
                                in (if v=true then "ff" else "tt")
                                end
                |        DataTypes.INVERSE(exp) => Rational.showDecimal(valOf(Rational.inverse(Rational.fromDecimal(evaluateExp(exp)))))
                |        DataTypes.AND(exp1, exp2) =>
                            if (evaluateExp(exp1)="tt" andalso evaluateExp(exp2)="tt") then "tt" else "ff"
                |        DataTypes.OR(exp1, exp2) =>
                            if (evaluateExp(exp1)="ff" andalso evaluateExp(exp2)="ff") then "ff" else "tt"
                |        DataTypes.NEG(exp) =>
                            if (isRational(evaluateExp(exp))=false) then
                                let 
                                        val v = BigInt.toString(BigInt.multiply(BigInt.fromString(evaluateExp(exp)), [~1,1]))
                                in 
                                        (TextIO.output(TextIO.stdOut, v);v)
                                end
                            else
                                let val v = Rational.showDecimal(Rational.multiply(Rational.fromDecimal(evaluateExp(exp)), ([~1,1],[1,1])))
                                in v
                                end
                |        DataTypes.NOT(exp) =>
                            if (evaluateExp(exp)="tt") then "ff" else "tt"
                
                                
            (* scopeStack:= (hd(!scopeStack)+1) :: (!scopeStack) ; *)
            fun process_block(blk)=
                let
                    val DataTypes.BLK(dec, com_list)=blk
                    val DataTypes.DEC(vardec_list,prodec_list)=dec

                    fun process_var_declarations([]) = ()
                    | process_var_declarations(vardec::vardecs)=
                        case vardec of
                           DataTypes.VARDEC(string_list, typ) => (insertInVarTable(string_list, (typ, !parent_id, " "));process_var_declarations(vardecs))

                    fun process_pro_declarations([])=()
                    | process_pro_declarations(procedure::procedures)=
                        case procedure of 
                            DataTypes.PRODEC(string, blk) => (insertInProcTable(string, (blk, !scope_id, !parent_id)); scope_id := !scope_id + 1 ; process_pro_declarations(procedures))

                    fun process_commands([])=()
                    |   process_commands(command::comseq)=
                            case command of 
                                DataTypes.Print(Type, Exp) => (TextIO.output(outs, evaluateExp(Exp)^"\n");process_commands(comseq))
                        |    DataTypes.SET(string,Exp) => (                                
                                let
                                val value = getVar(string, !parent_id)
                                val parent_id_req= #2(value)
                                val new_value=evaluateExp(Exp)
                                val rem = (HashTable.remove varTable (parent_id_req,string))
                                in
                                    insertInVarTable([string], (#1(value), #2(value), new_value))
                                end;
                                process_commands(comseq))
                        |    DataTypes.Read(string) => (
                                                            let
                                                                val str=valOf(TextIO.inputLine(TextIO.stdIn))
                                                                val new_value=substring(str, 0, size(str)-1)
                                                                val value = valOf(HashTable.find varTable (!parent_id,string))
                                                                val rem = (HashTable.remove varTable (!parent_id,string))
                                                            in
                                                              insertInVarTable([string], (#1(value), #2(value), new_value))  
                                                            end
                                                            ;process_commands(comseq))
                        |    DataTypes.Call(string) => (
                                                        let 
                                                        val value = getProcedure(string, !parent_id)
                                                        val next_blk = #1(value)
                                                        val original_parent_id = !parent_id
                                                        in
                                                            parent_id := #2(value);
                                                            process_block(next_blk);
                                                            parent_id := original_parent_id
                                                        end; 
                                                        process_commands(comseq))
                        |    DataTypes.ITE(Exp, coms1, coms2) => (if evaluateExp(Exp)="tt" then (process_commands(coms1)) else process_commands(coms2);process_commands(comseq))
                        |    DataTypes.WH(Exp, coms) => (whileCmd(Exp, coms); process_commands(comseq))
                    and whileCmd(Exp, commands)= if evaluateExp(Exp)="tt" then (process_commands(commands); whileCmd(Exp,commands)) else () 
                in
                  process_var_declarations(vardec_list);
                  process_pro_declarations(prodec_list);
                  process_commands(com_list)
                end;
        in
            process_block(blk);
            TextIO.closeOut outs
        end;
end;
