(* Proyecto 3 *)
(* Roberto Rojas Segnini *)
(* Daniel Alvarado Bonilla *)

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
	      | x::xs' => if Char.isUpper(String.sub(x,0)) then x::longest_capitalized_aux(xs') else longest_capitalized_aux(xs')
    in
	lcs xs'
    end
	
    

(*6*)	 
