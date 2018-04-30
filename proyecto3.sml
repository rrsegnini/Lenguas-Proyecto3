(* Proyecto 3 *)
(* Roberto Rojas Segnini *)
(* Daniel Alvarado Bonilla *)

(*fun only_capitals(xs)=
    case xs of
	[] => [] 
      | x::xs'  => if Char.isUpper(String.sub(x,0)) then x::only_capitals(xs')
		   else only_capitals(xs')*)
				     
fun only_capitals(xs)=
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs
