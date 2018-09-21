(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)
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
                                  | NONE => NONE    
                        end

fun get_substitutions1(lst, s) = 
    case lst of 
        [] => []
    |   head::tail =>   let
                            val hd_ans = all_except_option(s, head);
                        in
                            case hd_ans of
                                NONE => get_substitutions1(tail, s)
                              | SOME l => l @ get_substitutions1(tail, s)
                        end

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
      | head::tail =>   if head = c
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
        fun game(card_list, held_cards, move_list, goal) = 
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

val test1_0=all_except_option("5",["4", "5", "9", "10", "5"]) = SOME ["4", "9", "10", "5"];
val test2_0=get_substitutions1([["Bob","Robert"],["Betty", "Anne"],["Robbie","Robert","R"], ["Robby", "Bob", "Robert"]],
                               "Robert")
            = ["Bob","Robbie","R", "Robby", "Bob"];
val test3_0=get_substitutions2([["Bob","Robert"],["Betty", "Anne"],["Robbie","Robert","R"], ["Robby", "Bob", "Robert"]],
                               "Robert")
            = ["Bob","Robbie","R", "Robby", "Bob"];
val test4_0=similar_names([
                             ["Bob","Robert"],
                             ["Robbie","Robert","R"],
                             ["Robby", "Bob", "Robert"],
                             ["Betty", "Anne"]
                         ], {first="Robert", middle="J", last="Willis"}) =
            [{first="Robert", middle="J", last="Willis"},
            {first="Bob", middle="J", last="Willis"},
             {first="Robbie", middle="J", last="Willis"},
             {first="R", middle="J", last="Willis"},
             {first="Robby", middle="J", last="Willis"},
             {first="Bob", middle="J", last="Willis"}];
val test5_0= card_color((Hearts, Num 8)) = Red;
val test6_0= card_value((Hearts, Num 8)) = 8;
val test7_0 = remove_card([(Hearts, Num 8), (Spades, Ace), (Clubs, Num 2), (Diamonds, King)], (Clubs, Ace), IllegalMove) = [(Hearts, Num 8), (Spades, Ace), (Clubs, Num 2), (Diamonds, King)] handle IllegalMove => true;
 val test8_0 = all_same_color([(Hearts, Num 8)]) = true;
val test9_0 = sum_cards([(Hearts, Num 8), (Spades, Ace), (Clubs, Num 2), (Diamonds, King)]) = 31;
val test10_0 = score([(Hearts, Num 8), (Spades, Ace), (Clubs, Num 2), (Diamonds, King)], 1) = 30 * 2;
val test11_0 = officiate([(Hearts, Num 8), (Spades, Ace), (Clubs, Num 2), (Diamonds, King)], [Draw, Discard(Clubs, Num 3)], 10) = 14 handle IllegalMove => true; 

