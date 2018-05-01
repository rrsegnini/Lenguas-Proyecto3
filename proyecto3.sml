(* Proyecto 3 *)
(* Roberto Rojas Segnini *)
(* Daniel Alvarado Bonilla *)
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
    let val lsh = longest_string_helper (fn (x, x') =>  if String.size(x)>String.size(x') then true else false)
    in
	lsh xs
    end
			  

(*5*)
fun longest_capitalized(xs)=
    let val lcs = longest_string_helper (fn (x, x') =>  if String.size(x')>String.size(x) then false else true)
	val xs' = longest_capitalized_aux(xs)
				     
	fun longest_capitalized_aux(xs) = 
	    case xs of
		[] => [] 
	      | x::xs' => if (Char.isUpper o String.sub) (x,0) then x::longest_capitalized_aux(xs') else longest_capitalized_aux(xs')
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
	      | x::xs' => if f x = NONE then NONE else all_answers_aux(f, xs', (valOf (f x))@some_lst)
    in
	all_answers_aux(f, xs, [])
    end
	
	
