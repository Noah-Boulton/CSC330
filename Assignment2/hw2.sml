(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)
(* Write a function all_except_option, which takes a string and a string list. 
Return NONE if the string is not in the list, else return SOME list where 
lst is identical to the argument list except the string is not in it. 
You may assume the string is in the list at most once. Use same_string, 
provided to you, to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(s1, lst) =
    case lst of 
        [] => NONE
    |   head::tail =>   let 
                            val tl = all_except_option(s1, tail);
                        in  
                            if same_string(s1, head)
                            then SOME tail
                            else 
                                case tl of
                                    SOME l => SOME (head :: l)
                                |   NONE => NONE    
                        end

(*Write a function get_substitutions1, which takes a string list list (a list of list of
strings, the substitutions) and a string s and returns a string list. The result has all the
strings that are in some list in substitutions that also has s, but s itself should not be in the result.*)
fun get_substitutions1(lst, s) = 
    case lst of 
        [] => []
    |   head::tail =>   let
                            val hd_ans = all_except_option(s, head);
                            val tl_ans = get_substitutions1(tail, s) (* Might not need this here because it should only be calculated once*)
                        in
                            case hd_ans of
                                NONE => tl_ans
                            |   SOME l => l @ tl_ans
                        end

(* fun get_substitutions2(lst, s) = 
    case lst of 
        [] => []
    |   head::tail =>   let
                            val hd_ans = all_except_option(s, head);
                            val tl_ans = get_substitutions2(tail, s)
                        in
                            case hd_ans of
                                NONE => tl_ans
                            |   SOME l => l @ tl_ans
                        end *)

fun get_substitutions2(lst, s) = 
    let 
        fun subs(lst, s, result) = 
            case lst of 
                [] => result    
              | head::tail =>   let
                                    val hd_ans = all_except_option(s, head);
                                in
                                    case hd_ans of
                                        NONE => subs(tail, s, result)
                                      | SOME l => subs(tail, s, result @ l)
                                end
    in  
        subs(lst, s, [])
    end


fun similar_names(lst, {first:string ,middle:string , last:string}) = 
    let
      val names = first :: get_substitutions2(lst, first);
      fun construct_names(lst, {first:string ,middle:string , last:string}) = 
        case lst of 
            [] => []
        |   head::tail => {first = head,middle = middle, last = last} :: construct_names(tail, {first = first,middle = middle, last = last})
    in
        construct_names(names, {first = first ,middle = middle , last = last})
    end

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove


(* put your solutions for Part 2 here *)
fun card_color((s, r):card) = 
    case s of 
        Diamonds => Red
      | Hearts => Red
      | Spades => Black
      | Clubs => Black

fun card_value((s, r):card) = 
    case r of 
        King => 10
      | Queen => 10
      | Jack => 10
      | Ace => 11
      | Num(x) => x

fun remove_card(cs: card list, c: card, e: exn) = 
    case cs of 
        [] => raise e
    |   head::tail =>   if head = c
                        then tail
                        else head::remove_card(tail,c ,e)

fun all_same_color(cs) = 
    case cs of
        head::(neck::tail) =>   if card_color(head) = card_color(neck)
                                then all_same_color(neck::tail)
                                else false
      | _ => true

fun sum_cards(cs) = 
    let 
        fun sum(cs, s) = 
            case cs of
                [] => s
              | head::tail => sum(tail, s + card_value(head))
    in
        sum(cs, 0)
    end

(*Let sum be the sum of the values of the held-cards. 
If sum is greater than goal, the preliminary score is two times (sum − goal), 
else the preliminary score is (goal − sum). 
The score is the preliminary score unless all the held-cards are the same color, 
in which case the score is the preliminary score divided by 2 
(and rounded down as usual with integer division; use ML’s div operator).*)
fun score(held_cards, goal) = 
    let
        val sum = sum_cards(held_cards);
        val color = all_same_color(held_cards);
        val preliminary_score = if sum > goal
                                then 2*(sum - goal)
                                else (goal - sum);
    in  
        case color of 
            true  => preliminary_score div 2
          | false => preliminary_score
    end

fun officiate(card_list, move_list, goal) =
    let 
        fun game(card_list, held_cards, move_list, goal) = (*Need to extract the move_list move*)
            case move_list of   
                [] => score(held_cards, goal)
                | move::tail => case move of 
                                Draw => (case card_list of     
                                        [] => score(held_cards, goal)
                                      | c::tl =>  if(sum_cards(c::held_cards) > goal)
                                                    then score(c::held_cards, goal)
                                                    else game(tl, c::held_cards, tail, goal))

                              | Discard(c) => game(card_list, remove_card(held_cards, c, IllegalMove), tail, goal)
    in 
        game(card_list, [], move_list, goal)
    end