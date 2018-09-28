(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

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

(* Description of g:

*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)
fun only_capitals l = 
	List.filter (fn s => Char.isUpper(String.sub(s, 0))) l

fun longest_string1 l = 
	foldl (fn (s, s2) => if String.size(s) > String.size(s2) then s else s2) "" l

fun longest_string2 l  = 
	foldl (fn (s, s2) => if String.size(s) >= String.size(s2) then s else s2) "" l

fun longest_string_helper f = fn acc => fn l =>
	foldl f acc l

val longest_string3 = longest_string_helper (fn (s, s2) => if String.size(s) > String.size(s2) then s else s2) "" 
val longest_string4 = longest_string_helper (fn (s, s2) => if String.size(s) >= String.size(s2) then s else s2) "" 

val longest_capitalized = longest_string3 o only_capitals;

val rev_string = implode o rev o explode;

fun first_answer f l =
	let
		val li = map f l;
		val li2 = List.filter (fn v => isSome(v)) li;
	in 
		case li2 of
			[] => raise NoAnswer
		  | v::vs => valOf(v)
	end

fun all_answers f l =
	let 
		fun append (lst, acc) =
			case lst of
				[] => SOME acc
			  | head::tail => 	if(isSome(head))
			  					then append(tail, acc @ valOf(head))
								else NONE
	in	
		append(map f l, [])
	end