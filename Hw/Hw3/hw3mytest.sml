
val test_only_capitals_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test_only_capitals_2 = only_capitals ["AJ","aj","Charles"] = ["AJ","Charles"]
val test_only_capitals_3 = only_capitals ["sunny","bob","james"] = []
val test_only_capitals_4 = only_capitals ["Jesus","Peter","Parker"] = ["Jesus","Peter","Parker"]
val test_only_capitals_5 = only_capitals ["a","b","c"] = []

val test_longest_string1_1 = longest_string1 ["A","bc","C"] = "bc"
val test_longest_string1_2 = longest_string1 ["A","hello","C"] = "hello"
val test_longest_string1_3 = longest_string1 [] = ""
val test_longest_string1_4 = longest_string1 ["bc","C"] = "bc"
val test_longest_string1_5 = longest_string1 ["Helloa","B","Cc", "Hellob"] = "Helloa"

val test_longest_string2_1 = longest_string2 ["Aa","B","Cc", "B"] = "Cc"
val test_longest_string2_2 = longest_string2 ["Helloa","B","Cc", "Hellob"] = "Hellob"
val test_longest_string2_3 = longest_string2 ["a","b","cc", "dd"] = "dd"
val test_longest_string2_4 = longest_string2 ["James","Cory","Mary", "Michael"] = "Michael"
val test_longest_string2_5 = longest_string2 ["Testa","Testb","Testc"] = "Testc"

val test_longest_string3_1 = longest_string3 ["A","bc","C"] = "bc"
val test_longest_string3_2 = longest_string3 ["A","hello","C"] = "hello"
val test_longest_string3_3 = longest_string3 [] = ""
val test_longest_string3_4 = longest_string3 ["bc","C"] = "bc"
val test_longest_string3_5 = longest_string3 ["Helloa","B","Cc", "Hellob"] = "Helloa"

val test_longest_string4_1 = longest_string4 ["Aa","B","Cc", "B"] = "Cc"
val test_longest_string4_2 = longest_string4 ["Helloa","B","Cc", "Hellob"] = "Hellob"
val test_longest_string4_3 = longest_string4 ["a","b","cc", "dd"] = "dd"
val test_longest_string4_4 = longest_string4 ["James","Cory","Mary", "Michael"] = "Michael"
val test_longest_string4_5 = longest_string4 ["Testa","Testb","Testc"] = "Testc"


val test_longest_capitalized_1 = longest_capitalized ["A","bc","C"] = "A"
val test_longest_capitalized_2 = longest_capitalized ["A","hello","C"] = "A"
val test_longest_capitalized_3 = longest_capitalized [] = ""
val test_longest_capitalized_4 = longest_capitalized ["bc","C"] = "C"
val test_longest_capitalized_5 = longest_capitalized ["Helloa","B","Cc", "Hellob"] = "Helloa"

val test_rev_string_1 = rev_string "abc" = "cba"
val test_rev_string_2 = rev_string "hello" = "olleh"

val test_first_answer_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test_all_answers_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test_all_answers_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,3,4,5,6,7] = SOME [1]
val test_all_answers_3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,3,4,1,6,7] = SOME [1,1]
val test_all_answers_4 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,3,4,1,1,7] = SOME [1,1,1]
val test_all_answers_5 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test_count_wildcards_1 = count_wildcards Wildcard = 1
val test_count_wildcards_2 = count_wildcards (ConstructorP ("Hello", ConstructorP ("World", Wildcard))) = 1
val test_count_wildcards_3 = count_wildcards UnitP = 0
val test_count_wildcards_4 = count_wildcards (ConstP 5) =  0
val test_count_wildcards_5 = count_wildcards (TupleP [ConstructorP ("Hello", Wildcard), Wildcard]) = 2