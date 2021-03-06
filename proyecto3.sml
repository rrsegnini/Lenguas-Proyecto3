(* Proyecto 3 *)
(* Roberto Rojas Segnini *)
(* Daniel Alvarado Bonilla *)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard => f1 () 
	  | Variable x => f2 x
	  | TupleP ps  => List.foldl(fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _  => 0
    end

	
		     
			   
		   
exception NoAnswer 

(*1*)				     
fun only_capitals(xs)=
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs
    
    
(*2*)
fun longest_string1(xs)=
    foldl(fn (x, x') => if String.size(x')>=String.size(x) then x' else x) ""  xs

(*3*)
fun longest_string2(xs)=
    foldl(fn (x, x') => if String.size(x)>=String.size(x') then x else x') ""  xs


(*4*)
fun longest_string_helper f xs =
    case xs of
	[] => "" 
	   | x::[] => x 
	   | x::x'::xs' => if f(x,x') then  longest_string_helper f (x::xs') else  longest_string_helper f (x'::xs') 
			  
fun longest_string3(xs)=
    let val lsh = longest_string_helper (fn (x, x') =>  if String.size(x')>String.size(x) then false else true)
    in
	lsh xs
    end


fun longest_string4(xs)=
    let val lsh = longest_string_helper (fn (x, x') =>  if String.size(x)>String.size(x')
							then true
							else false)
    in
 	lsh xs
    end
			  

(*5*)
fun longest_capitalized(xs)=
    let val lcs = longest_string_helper (fn (x, x') =>  if String.size(x')>String.size(x)
							then false
							else true)

				     
	fun longest_capitalized_aux(xs) = 
	    case xs of
		[] => [] 
	      | x::xs' => if (Char.isUpper o String.sub) (x,0)
			  then x::longest_capitalized_aux(xs')
			  else longest_capitalized_aux(xs')

	val xs' = longest_capitalized_aux(xs)
					 
    in
	lcs xs'
    end
	
    

(*6*)
fun rev_string(s)=
    (String.implode o rev o String.explode) (s)



					    
(*7*)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
     | x::xs' => if f x = SOME(x) then x else first_answer f xs'
		  
    
					    
(*8*)
fun all_answers f xs =
    let fun all_answers_aux(f, xs, some_lst) = 
	    case xs of
		[] => SOME(some_lst)
	      | x::xs' => if f x = NONE
			  then NONE
			  else all_answers_aux(f, xs', (valOf (f x))@some_lst)
    in
	all_answers_aux(f, xs, [])
    end
	
	
(*9*)

	(*a*)
fun count_wildcards p =
    g (fn x => 1) (fn x2 => 0) p
    

      (*b*)
fun count_wild_and_variable_lengths(p)=
    g (fn x => 1) (fn x2 => String.size(x2) ) p


      (*c*)
fun count_some_var(s, p)=
   g (fn x => 0) (fn x2 => if x2 = s then 1 else 0 ) p 
				

(*10*)
fun check_pat(p)=
	let
		fun get_strings(p)=
			case p of Variable x => [x] 
				| TupleP xs => foldl( fn (x,x') => get_strings(x)@x') [] xs
				| ConstructorP xs=> [#1 xs]@get_strings(#2 xs)
				| ConstP x => [""]
				| UnitP => [""]
				| Wildcard => [""]
		fun all_diff(xs)=
		   foldl(fn (x, acc) => if acc = true then if List.exists(fn y => x = y) xs 
										then acc = false 
										else acc = true 
									else acc = false) false  xs
			
		val strings_list = get_strings p;

	in
	    all_diff strings_list
	end

(*11*)

fun match_aux(v,p)=
   (* let
	fun match_aux(v,p)=*)
    case (v,p) of
	(Tuple v',TupleP p') => case (v',p') of
				    (_::xs,Wildcard::xs') => match_aux(Tuple xs,TupleP xs')
				  | (Const x::xs, ConstP x'::xs') => if x=x'
								     then match_aux(Tuple xs,TupleP xs')
								     else match_aux(Tuple xs,TupleP xs')
				  | (Constructor(s,x)::xs,ConstructorP(s',x')::xs') => if s = s'
										       then match_aux(Tuple [x], TupleP [x'])
											    @match_aux(Tuple xs,TupleP xs')
												  else match_aux(Tuple xs,TupleP xs')
				  | (Tuple x::xs, TupleP x'::xs') =>
				    let
				      val ans = 5(*all_answers match (ListPair.zip(x, x'))*)
				    in
					if length x = length x'
					   andalso ans <> 0
					then match_aux(Tuple x, TupleP x')@match_aux(Tuple xs,TupleP xs')
					else match_aux(Tuple xs,TupleP xs')
				    end

				  |(x::xs, Variable x'::xs') => (x',x)::match_aux(Tuple xs, TupleP xs')
				  | _ => []
					
   (* in
	SOME(match(v,p))
    end*)
	










					     
    
    
	
