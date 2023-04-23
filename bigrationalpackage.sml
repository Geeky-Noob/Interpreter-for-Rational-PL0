signature BIGINT =
sig
  type bigint=int list
  exception error
  val zero : bigint
  val one : bigint
  val fromInt : int -> bigint
  val toInt : bigint -> int option
  val toString : bigint -> string
  val fromString: string->bigint
  val normalise: bigint -> bigint
  val negate : bigint -> bigint
  val compareDigits : bigint * bigint -> order
  val compare : bigint * bigint -> order
  val add3: int list*int list * int * int list->int list
  val add : bigint * bigint -> bigint
  val subtract : bigint * bigint -> bigint
  val multiply : bigint * bigint -> bigint
  val nextdigit: bigint*bigint*int->int
  val divide_helper : bigint * bigint *int list-> bigint * bigint
  val divide : bigint * bigint -> bigint * bigint
end

structure BigInt : BIGINT =
struct
  type bigint = int list
  exception error
  val zero = [1, 0]
  val one = [1, 1]
  fun fromInt n =
    if n < 0 then
      ~1 :: List.rev (List.map (fn c => Char.ord c - 48) (String.explode (Int.toString (~n))))
    else if n = 0 then zero
    else
      1 :: List.rev (List.map (fn c => Char.ord c - 48) (String.explode (Int.toString n)))
  fun toInt (sign :: digits) =
    if sign = ~1 then
      Option.map (fn n => ~n) (toInt digits)
    else
      let
        fun toInt' [] s = SOME s
          | toInt' (a :: T) s = toInt' T (s * 10 + a)
      in
        toInt' digits 0
      end
  fun fromString("")=[1,0]    
   | fromString(s)=
    let
      val l1=explode(s)
      fun f c=(Char.ord (c) - Char.ord (#"0"))
      val l2=List.map f l1
      val l3=List.map f (tl(l1))
    in
      if hd(l1) = #"~" then ~1::l3 else 1::l2
    end
  fun toString (sign :: digits) =(* list ko reverse nhi kiya hai *)
    (if sign = ~1 then "-" else "") ^ String.implode (List.map (fn d => Char.chr (d + 48)) digits)
  fun negate (sign :: digits) =
    if sign = ~1 then
      1::digits
    else if digits = [0] then zero
    else
      ~1 :: digits
  fun normalise (s::[0])=[1,0]
   | normalise(s::[])=[1,0]
   | normalise(s::digits)=
    if hd(digits)=0 then normalise(s::tl(digits))
    else s::digits
  fun compareDigits ([], []) = EQUAL(*note compare digits in reverse*)
            (* | compareDigits ([], _) = LESS
            | compareDigits (_, []) = GREATER *)
            | compareDigits (x::xs, y::ys) =
                if x > y then GREATER
                else if x < y then LESS
                else compareDigits (xs, ys)
  fun compare (s1::d1, s2::d2) =
        let
            val (sign1::x1) = normalise(s1::d1)
            val (sign2::x2) = normalise(s2::d2)
            val n1 = length x1
            val n2 = length x2
        in
            if s1 > s2 then GREATER
            else if s1 < s2 then LESS
            else if n1 > n2 andalso s1=1 then GREATER
            else if n1 > n2 andalso s1= ~1 then LESS
            else if n1 < n2 andalso s1=1 then LESS
            else if n1 < n2 andalso s1= ~1 then GREATER
            else 
                let
                    val x=compareDigits (x1, x2)
                in 
                    if s1= ~1 andalso x=GREATER then LESS
                    else if s1= ~1 andalso x=LESS then GREATER
                    else x
                end
        end
  fun add3 ([], [], carry,dig) = dig 
        | add3 (x::T1, [], carry,dig) = if (x+carry) >= 0 then add3 (T1, [], 0,(x+carry)::dig) else add3 (T1, [], ~1,(x+carry+10)::dig)
        | add3 (x :: T1, y :: T2, carry,dig) =
            let
                val c=if x<y then 10 else 0 
                val sum = x - y + c
                val (digit, carry') = if c = 0 then (sum , 0) else (sum, ~1)
            in
                add3 (T1, T2, carry',digit::dig)
            end
  fun add (s1::d1, s2::d2) =
    let
      fun add2 ([], [], carry,dig) = if carry = 0 then dig else carry::dig
        | add2 ([], y::T2, carry,dig) = if (y+carry) > 9 then add2 ([], T2, 1,(y+carry- 10)::dig) else add2 ([], T2, 0,(y+carry)::dig)
        | add2 (x::T1, [], carry,dig) = if (x+carry) > 9 then add2 (T1, [], 1,(x+carry- 10)::dig) else add2 (T1, [], 0,(x+carry)::dig)
        | add2 (x :: T1, y :: T2, carry,dig) =
            let
                val sum = x + y + carry
                val (digit, carry') = if sum > 9 then (sum - 10, 1) else (sum, 0)
            in
                add2 (T1, T2, carry',digit::dig)
            end
      fun add3 ([], [], carry,dig) = dig 
        | add3 (x::T1, [], carry,dig) = if (x+carry) >= 0 then add3 (T1, [], 0,(x+carry)::dig) else add3 (T1, [], ~1,(x+carry+10)::dig)
        | add3 (x :: T1, y :: T2, carry,dig) =
            let
                val c=if x<y then 10 else 0 
                val sum = x - y + c + carry
                val (digit, carry') = if c = 0 then (sum , 0) else (sum, ~1)
            in
                add3 (T1, T2, carry',digit::dig)
            end
      fun sign(s1::d1,s2::d2)= 
        if s1=s2 then s1 
        else if compare(1::d1,1::d2)=GREATER andalso s1=1 then 1
        else if compare(1::d1,1::d2)=GREATER andalso s1= ~1 then ~1 
        else if compare(1::d1,1::d2)=LESS andalso s1=1 then ~1
        else if compare(1::d1,1::d2)=LESS andalso s1= ~1 then 1 
        else 1
      val (sign1::x1) = normalise(s1::d1)
      val (sign2::x2) = normalise(s2::d2)
      val s=sign(s1::x1,s2::x2)
    in
      if sign1=sign2 then normalise(s :: add2 (List.rev x1, List.rev x2, 0,[]))
      else if compare(1::x1,1::x2)=GREATER then normalise(s :: add3 (List.rev x1, List.rev x2, 0,[]))
      else if compare(1::x1,1::x2)=LESS then normalise(s :: add3 (List.rev x2, List.rev x1, 0,[]))
      else [1,0]
    end
  fun subtract(s1::d1,s2::d2)= add(s1::d1,~(s2)::d2)
  fun multiply(s1::d1,s2::d2)=
    let
      fun mul([],x,carry,res)=if carry>0 then carry::res else res
        | mul(d1,x,carry,res)=
            let
                val dig=(List.last d1)*x + carry
            in 
                mul(List.take(d1,length d1-1),x,dig div 10,[dig mod 10]@res)
            end
      fun prod(d1,[])=[0]
        | prod(d1,d2)= tl((add(1::(mul(d1, List.last d2, 0, [])),1::(prod(d1,List.take(d2,length d2-1))@[0]))))
      val (sign1::x1)=normalise(s1::d1)
      val (sign2::x2)=normalise(s2::d2)
    in
      if s1=s2 then 1:: prod(x1,x2)
      else ~1::prod(x1,x2)
    end
  fun nextdigit(l1, l2, d): int =
                if compare(1::l1,multiply(1::l2,1::[d])) = LESS then (d-1)
                else if d = 9 then 9
                else nextdigit(l1,l2,d+1)
  fun divide_helper (p,q,l) =
        let 
            fun nextdigit(l1, l2, d): int =
                if compare(1::l1,multiply(1::l2,1::[d])) = LESS then (d-1)
                else if d = 9 then 9
                else nextdigit(l1,l2,d+1)
        in 
            if p=[] then ([],l)
            else 
                let
                    val new_l=tl(normalise(1::l@[hd p]))
                    val d = nextdigit(new_l,q,0)
                    val dif = tl (subtract(1::new_l,multiply(1::[d],1::q)))
                    val ans = divide_helper(tl p,q,dif)
                in
                    (d::(#1(ans)),(#2(ans)))
                end
        end 
  fun divide(_,[1,0])=raise error
   | divide (a::x,b::y) =
            let 
                val (a1::p) = normalise (a::x)
                val (b1::q) = normalise (b::y)
                val ans = divide_helper(p,q,[])
                val quotient = (a1*b1) :: tl (normalise(1::(#1(ans))))
                val remainder = normalise(a1 :: (#2(ans)))
            in 
                (quotient,remainder)
            end
end

signature RATIONAL = sig
    type bigint=BigInt.bigint
    type rational
    exception rat_error
    val gcd: bigint*bigint->bigint
    val make_rat: bigint * bigint -> rational option
    val rat: BigInt.bigint -> rational option
    val reci: BigInt.bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational option (* division *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
end

functor Rat(BigInt : BIGINT) : RATIONAL = struct
    type bigint=BigInt.bigint
    type rational = BigInt.bigint*BigInt.bigint
    exception rat_error
    fun gcd(x:BigInt.bigint, y:BigInt.bigint) =
        if BigInt.compare(BigInt.zero, y)=EQUAL then x
        else gcd(y, #2(BigInt.divide(x, y)))
    fun make_rat(nr:BigInt.bigint, dr:BigInt.bigint):rational option=
        if BigInt.compare(BigInt.zero, dr)=EQUAL then NONE
        else let
                val g = gcd(1::tl(nr), 1::tl(dr))
             in
                if hd(nr)=hd(dr) then SOME (#1(BigInt.divide(1::tl(nr), g)), #1(BigInt.divide(1::tl(dr), g)))
                else SOME (BigInt.negate(#1(BigInt.divide(1::tl(nr), g))), #1(BigInt.divide(1::tl(dr), g)))
             end
    fun rat(a: BigInt.bigint) = make_rat(a, BigInt.one)
    fun reci(a: BigInt.bigint) = make_rat(BigInt.one, a)
    fun neg(a: rational) = 
        let
          val (nr,dr) = a
          val (n1,d1)=valOf(make_rat(nr,dr))
        in
          (BigInt.negate(n1), d1)
        end
    fun inverse(a: rational) = 
        let
          val (nr, dr) = a
        in
          if BigInt.compare(BigInt.zero, nr)=EQUAL then NONE
          else make_rat(dr,nr)
        end
    fun equal(r1:rational, r2:rational) =
        let
          val (nr1, dr1) = r1
          val (nr2, dr2) = r2
        in
          BigInt.compare(BigInt.multiply(nr1,dr2), BigInt.multiply(nr2,dr1))=EQUAL
        end
    fun less(r1:rational, r2:rational) =
        let
          val (nr1, dr1) = r1
          val (nr2, dr2) = r2
        in
          BigInt.compare(BigInt.multiply(nr1,dr2), BigInt.multiply(nr2,dr1))=LESS
        end
    fun add(r1, r2) =
        let
            val (nr1, dr1) = r1
            val (nr2, dr2) = r2
            val nr = BigInt.add(BigInt.multiply(nr1, dr2), BigInt.multiply(nr2, dr1))
            val dr = BigInt.multiply(dr1, dr2)
        in
            case make_rat (nr, dr) of
                SOME r => r
            | NONE => raise rat_error
        end
    fun subtract(r1, r2) =
        let
            val (nr1, dr1) = r1
            val (nr2, dr2) = r2
            val nr = BigInt.subtract(BigInt.multiply(nr1, dr2), BigInt.multiply(nr2, dr1))
            val dr = BigInt.multiply(dr1, dr2)
        in
            case make_rat (nr, dr) of
                SOME r => r
            | NONE => raise rat_error
        end
    fun multiply(r1, r2) =
        let
            val (nr1, dr1) = r1
            val (nr2, dr2) = r2
            val nr = BigInt.multiply(nr1, nr2)
            val dr = BigInt.multiply(dr1, dr2)
        in
            case make_rat (nr, dr) of
                SOME r => r
            | NONE => raise rat_error
        end
    fun divide(r1:rational, r2:rational) = 
        let
            val (nr1, dr1) = r1
            val (nr2, dr2) = r2
            val nr = BigInt.multiply(nr1, dr2)
            val dr = BigInt.multiply(dr1, nr2)
        in
          make_rat(nr,dr)
        end
    fun showRat (r:rational) =
        let
            val (nr, dr)=r
            val reduced = make_rat (nr,dr)
        in
            case reduced of
                NONE => raise rat_error
            | SOME (nr,dr) => 
                (BigInt.toString nr) ^ "/" ^ (BigInt.toString dr)
        end
    fun frac_digs(l,rem,quo,d) =
                    let 
                        fun f (x: int list):bool = if rem = x then true else false 
                    in 
                        if List.find f l = NONE then 
                            let 
                                val a = BigInt.divide_helper(tl(BigInt.normalise(1::(rem @ [0]))),d,[])
                                val quo_next = tl(BigInt.normalise(1:: #1(a)))
                                val rem_next = tl(BigInt.normalise(1:: #2(a)))
                            in 
                                frac_digs(l @ [rem],rem_next,quo @ quo_next,d)
                            end 
                        else (l,rem,quo)
                    end 
    fun showDecimal (r) =
        let 
            val SOME p = make_rat(r)
            fun combine(l,s)=
              if List.length l=0 then s
              else combine(tl(l),s^(Int.toString(hd(l))))
        in 
            if make_rat(r) = NONE then raise rat_error
            else
            let  
                fun index(l,x,i) = if List.hd l = x then i else index(List.tl l ,x,i+1)
                val (s::n,1::d) = p
                val sign=if s=1 then "" else "~"
                val (quo,rem) = BigInt.divide_helper(n,d,[])
                val i_part = combine(tl (BigInt.normalise (1::quo)),"")
                val (l,r,q) = frac_digs([],rem,[],d)
                val i = index(l,r,0)
                val nonr = combine(List.take(q,i),"")
                val recur = combine(List.drop(q,i),"")  
            in 
              sign^i_part^"."^nonr^"("^recur^")"
            end
        end
    fun fromDecimal(s)=
      let
        val sign=if String.sub(s,0)= #"~" then ~1 else 1
        val new_s=if String.sub(s,0)= #"~" orelse String.sub(s,0)= #"+" then implode(tl(explode(s))) else s
        val fields = String.fields (fn c => c = #"." orelse c = #"(" orelse c = #")") new_s
        val SOME i=rat(BigInt.fromString(List.nth(fields,0)))
        val l1=String.size(List.nth(fields,1))
        val l2=String.size(List.nth(fields,2))
        fun denom1(l)=if l>0 then (9::denom1(l-1)) else []
        fun denom2(l)=if l>0 then (0::denom2(l-1)) else []
        val nr=BigInt.subtract(BigInt.fromString(List.nth(fields,1)^List.nth(fields,2)),BigInt.fromString(List.nth(fields,1)))
        val dr=[1]@denom1(l2)@denom2(l1)
        val SOME j=make_rat(nr,dr)
      in
        if List.length fields=4 then
          if sign= ~1 then add(neg(i),neg(j)) else add(i,j)
        else raise rat_error
      end
    fun toDecimal(r)=showDecimal(r)
end;
structure Rational=Rat(BigInt);