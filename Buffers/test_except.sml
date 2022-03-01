fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s, ls) =
    case ls of
       [] => NONE
     | x::xs => case same_string (x, s) of
                 true => SOME (xs)
                 | false => case all_except_option (s, xs) of
                             NONE => NONE
                             | SOME y => SOME (x::y)

val all_except_option_test1 = all_except_option ("i", ["i"]) = SOME []
val all_except_option_test2 = all_except_option ("i", ["am"]) = NONE
val all_except_option_test3 = all_except_option ("i", ["i", "am", "iron", "man"]) = SOME ["am", "iron", "man"]
val all_except_option_test4 = all_except_option ("am", ["i", "am", "iron", "man"]) = SOME ["i", "iron", "man"]

