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
	g takes 2 functions:
		f1 : unit -> int
		f2 : string -> int
	g computes f1 of all wildcards and f2 of all strings.
	The result is summed for lists of patterns by using a partial application of g.
	g returns 0 for UnitP and ConstP, effectively ignoring them. 
*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 () (*unit -> int*)
	  | Variable x        => f2 x (*x of string -> int*)
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps (*pattern list*)
	  | ConstructorP(_,p) => r p (*any string * pattern -> int*)
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

fun count_wildcards p  = 
	g (fn ()=> 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
	g (fn ()=> 1) (fn x => String.size x) p

fun count_some_var (s, p) =
	g (fn () => 0) (fn x => if s = x then 1 else 0) p

(* fun check_pat p = 
	let 
		fun strings_of_pat p = 
			case p of	
			  	Variable x 	=> [x]
			  | TupleP ps 	=> List.foldl (fn (q,i) => (strings_of_pat q) @ i) [] ps
			  | ConstructorP(_,q) => strings_of_pat q
              | _ => []
		fun unique xs = 
			case xs of	
				[] => true
			  | x::xs => if List.exists (fn y => x = y) xs then false else unique(xs)
	in
		unique(strings_of_pat p)
	end *)

val check_pat  = 
	let 
		fun strings_of_pat p = 
			case p of	
			  	Variable x 	=> [x]
			  | TupleP ps 	=> List.foldl (fn (q,i) => (strings_of_pat q) @ i) [] ps
			  | ConstructorP(_,q) => strings_of_pat q
              | _ => []
		fun unique xs = 
			case xs of	
				[] => true
			  | x::xs => if List.exists (fn y => x = y) xs then false else unique(xs)
	in
		unique o strings_of_pat 
	end

fun match (v, p) = 
	case (v, p) of 
		(_, Wildcard) => SOME []
	  | (_, Variable s) => SOME [(s, v)]
	  | (Unit, UnitP) => SOME []
	  | (Const x, ConstP y) => if x = y then SOME [] else NONE
	  | (Tuple vs, TupleP ps) => if length vs = length ps then all_answers match (ListPair.zip(vs, ps)) else NONE
	  | (Constructor(s2, x) ,ConstructorP (s1, y)) => if s1 = s2 then match (x, y) else NONE
	  | (_, _) => NONE

fun first_match v ps = 
	SOME (first_answer (fn p => match(v, p)) ps)
	handle NoAnswer => NONE