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

val test_all_except_option_1 = all_except_option ("i", ["i"]) = SOME []
val test_all_except_option_2 = all_except_option ("i", ["am"]) = NONE
val test_all_except_option_3 = all_except_option ("i", ["i", "am", "iron", "man"]) = SOME ["am", "iron", "man"]
val test_all_except_option_4 = all_except_option ("am", ["i", "am", "iron", "man"]) = SOME ["i", "iron", "man"]
val test_all_except_option_5 = all_except_option ("ant", ["black", "ant", "iron"]) = SOME ["black", "iron"]
    
fun get_substitutions1 (lls, find) =
    case lls of
       [] => []
     | ls::lls' => case all_except_option (find, ls) of
                      NONE => get_substitutions1 (lls', find)
                    | SOME y => y @ get_substitutions1 (lls', find)

val test_get_substitutions1_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test_get_substitutions1_2 = get_substitutions1 ([["i", "am", "iron"],["black", "widow", "iron"]], "iron") = ["i", "am", "black", "widow"]
val test_get_substitutions1_3 = get_substitutions1 ([["i", "am", "iron"],["black", "widow", "iron"]], "hawk") = []
val test_get_substitutions1_4 = get_substitutions1 ([["i", "am", "iron"],["ant", "widow", "iron"]], "widow") = ["ant", "iron"]
val test_get_substitutions1_5 = get_substitutions1 ([["i"],["ant", "widow"], ["iron", "ant"]], "ant") = ["widow", "iron"]

(* fun get_substitutions2 (lls, find) =
   let fun aux (lls, acc) =
       case lls of
          [] => acc
        | ls::lls' => case all_except_option (find, ls) of
                         NONE => aux (lls', acc)
                       | SOME y => aux (lls', acc @ y)
   in aux (lls, [])
   end *)

fun get_substitutions2 (lls, find) =
    let fun aux (lls, acc) =
        case lls of
           [] => acc
         | ls::lls' => case all_except_option (find, ls) of
            NONE => aux (lls', acc)
          | SOME y => aux (lls', acc @ y)
    in aux (lls, [])
    end 

val test_get_substitutions2_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test_get_substitutions2_2 = get_substitutions2 ([["i", "am", "iron"],["black", "widow", "iron"]], "iron") = ["i", "am", "black", "widow"]
val test_get_substitutions2_3 = get_substitutions2 ([["i", "am", "iron"],["black", "widow", "iron"]], "hawk") = []
val test_get_substitutions2_4 = get_substitutions2 ([["i", "am", "iron"],["ant", "widow", "iron"]], "widow") = ["ant", "iron"]
val test_get_substitutions2_5 = get_substitutions2 ([["i"],["ant", "widow"], ["iron", "ant"]], "ant") = ["widow", "iron"]

(* Returns a list of full names you can produce by substituting for the first name (and only the first name) 
   using substitutions in part b or c *)
fun similar_names (lls, name) =
    let fun aux (ls, acc) =
        case ls of
           [] => acc
         | s::lls' => aux (lls', acc @ [{first=s, middle=(#middle name), last=(#last name)}])
    in aux (get_substitutions2 (lls, #first name), [name])
    end

val test_similar_names_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
        [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
         {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test_similar_names_2 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Elizabeth", middle="W", last="Smith"}) =
        [{first="Elizabeth", last="Smith", middle="W"}, {first="Betty", last="Smith", middle="W"}]
val test_similar_names_3 = similar_names ([["James","Jameson", "Jamie"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="James", middle="W", last="Smith"}) =
        [{first="James", last="Smith", middle="W"}, {first="Jameson", last="Smith", middle="W"}, {first="Jamie", last="Smith", middle="W"}]
val test_similar_names_4 = similar_names ([["James","Jameson", "Jamie"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Molly", middle="W", last="Smith"}) =
        [{first="Molly", last="Smith", middle="W"}]
val test_similar_names_5 = similar_names ([["James"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="James", middle="W", last="Smith"}) =
        [{first="James", last="Smith", middle="W"}]

(* put your solutions for problem 2 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
