(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Takes string and a string list, return NONE if the string is not in the list
   Else return all other elements *)
fun all_except_option (find, ls) =
    case ls of
       [] => NONE
     | s::ls' => case same_string (find, s)  of
                    true => SOME ls'
                  | false => case all_except_option (find, ls') of
                                NONE => NONE
                              | SOME y => SOME (s::y)

fun get_substitutions1 (lls, find) =
    case lls of
       [] => []
     | ls::lls' => case all_except_option (find, ls) of
                      NONE => get_substitutions1 (lls', find)
                    | SOME y => y @ get_substitutions1 (lls', find)


fun get_substitutions2 (lls, find) =
    let fun aux (lls, acc) =
        case lls of
           [] => acc
         | ls::lls' => case all_except_option (find, ls) of
            NONE => aux (lls', acc)
          | SOME y => aux (lls', acc @ y)
    in aux (lls, [])
    end 


(* Returns a list of full names you can produce by substituting for the first name (and only the first name) 
   using substitutions in part b or c *)
fun similar_names (lls, name) =
    case name of
       {first=a, middle=b, last=c} => let fun aux (ls, acc) =
                                              case ls of
                                                 [] => acc
                                               | s::ls' => aux (ls', acc @ [{first=s, middle=b, last=c}])
                                      in aux (get_substitutions2 (lls, a), [name])
                                      end
     


(* put your solutions for problem 2 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (card) =
    case card of
       (Spades, _) => Black
     | (Clubs, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red

fun card_value (card) =
    case card of
       (_, Num i) => i
     | (_, Jack) => 10
     | (_, Queen) => 10
     | (_, King) => 10
     | (_, Ace) => 11

fun remove_card (cs, c, e) =
   case cs of
      [] => raise e
    | x::cs' => case x = c of
                  true => cs'
                | false => case remove_card (cs', c, e) of
                              [] => []
                            | y => x::y
    
fun all_same_color (cs) =
    case cs of
       [] => true
     | y::[] => true
     | x::y::cs' => case card_color (x) = card_color (y) of
                       true => all_same_color (y::cs')
                     | false => false

fun sum_cards (cs) =
    let fun aux (cs, acc) =
        case cs of
           [] => acc
         | c::cs' => aux (cs', card_value (c) + acc)
    in aux (cs, 0)
    end

(* If sum is greated than goal, the prem score is 3 x (sum - goal) 
   Else the prem score is (goal - sum)
   Score is prem unless all same color
   If not prem, score is div 2 (and rounded down) *)
fun score (cs, goal) =
    let val sum = sum_cards (cs)
        val prem_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in  if all_same_color (cs) then prem_score div 2 else prem_score
    end

(* Returns score at the end of the game after processing (some or all) of the moves 
   Recall datatype move = Discard of card | Draw  *)
fun officiate (cs, ms, goal) = 
    let fun state (cards, moves, helds) =
            case sum_cards (helds) > goal of
               true => score (helds, goal)
             | false => case moves of
                           [] => score (helds, goal) (* If there are no moves, game over, return score*)
                         | move::moves' => case move of (* Check what is the move *)
                                            Discard x => state (cards, moves', remove_card (helds, x, IllegalMove))
                                          | Draw => case cards of
                                                        [] => score (helds, goal)
                                                      | card::cards' => state (cards', moves', card::helds)
    in  state (cs, ms, [])
    end
