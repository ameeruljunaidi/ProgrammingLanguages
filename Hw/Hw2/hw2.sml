(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Return NONE if the string is not int he list
   Else return SOME lst, which is the arrgument *)
fun all_except_option (s, ls) =
    (* First case check is to check if the list is empty or not
       If empty, then just return NONE
       If not, check if the the first element is the string to find
       Then move on the to second case check  *)
    case ls of
       [] => NONE
                (* Second case check is to see if the first element is the string to find
                   If it is, then just return the rest of the list (xs)
                   If it is not, then move on the third case check to see what is the ouput
                   When the all_except_option is passed on to the rest of the list (xs) *)
     | f::r => case same_string (f, s) of
                    true => SOME (r)
                             (* If the rest of the list return NONE, then there's no string equal to the
                                string to find
                                If it returns SOME list, then that means the string was found
                                Cons on the current f to the rest of the list *)
                  | false => case all_except_option (s, r) of
                                NONE => NONE
                              | SOME y => SOME (f::y)
    

val all_except_option_test1 = all_except_option ("i", ["i"])
val all_except_option_test2 = all_except_option ("i", ["am"])
val all_except_option_test3 = all_except_option ("i", ["i", "am", "iron", "man"])
val all_except_option_test4 = all_except_option ("am", ["i", "am", "iron", "man"])

