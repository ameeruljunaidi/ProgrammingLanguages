
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
val test_all_answers_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,3,4,5,6,7] = NONE
val test_all_answers_3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,3,4,1,6,7] = NONE
val test_all_answers_4 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1] = SOME [1,1,1]
val test_all_answers_5 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []


val test_count_wildcards_1 = count_wildcards Wildcard = 1
val test_count_wildcards_2 = count_wildcards (ConstructorP ("Hello", ConstructorP ("World", Wildcard))) = 1
val test_count_wildcards_3 = count_wildcards UnitP = 0
val test_count_wildcards_4 = count_wildcards (ConstP 5) =  0
val test_count_wildcards_5 = count_wildcards (TupleP [ConstructorP ("Hello", Wildcard), Wildcard]) = 2

val test_count_wild_and_variable_lengths_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test_count_wild_and_variable_lengths_2 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "Hello"]) = 6
val test_count_wild_and_variable_lengths_3 = count_wild_and_variable_lengths (TupleP [Wildcard, UnitP]) = 1
val test_count_wild_and_variable_lengths_4 = count_wild_and_variable_lengths (TupleP []) = 0
val test_count_wild_and_variable_lengths_5 = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard, Variable "Hello"]) = 7

val test_count_some_var_1 = count_some_var ("x", Variable("x")) = 1
val test_count_some_var_2 = count_some_var ("x", TupleP [Variable "x", Variable "x"]) = 2
val test_count_some_var_3 = count_some_var ("x", Variable("s")) = 0
val test_count_some_var_4 = count_some_var ("x", Wildcard) = 0
val test_count_some_var_5 = count_some_var ("x", ConstructorP ("Test", Variable "x")) = 1

val test_check_pat_1 = check_pat (Variable("x")) = true
val test_check_pat_2 = check_pat (TupleP [Variable ("a"), Variable ("b"), Variable ("c")]) = true
val test_check_pat_3 = check_pat (TupleP [Variable ("a"), Variable ("a"), Variable ("b")]) = false
val test_check_pat_4 = check_pat (TupleP [Variable ("a"), Variable ("a"), Variable ("a")]) = false
val test_check_pat_5 = check_pat (TupleP []) = true

val test_match1_1 = match1 (Const(1), UnitP) = NONE
val test_match1_2 = match1 (Constructor ("Test", Const 4), ConstructorP ("Test", ConstP 4)) = SOME []
val test_match1_3 = match1 (Unit, Variable "Test") = SOME [("Test", Unit)]

val test_match_1 = match (Const(1), UnitP) = NONE
val test_match_2 = match (Constructor ("Test", Const 4), ConstructorP ("Test", ConstP 4)) = SOME []
val test_match_3 = match (Unit, Variable "Test") = SOME [("Test", Unit)]
val test_match_4 = match (Tuple [Const 17,  Unit,     Const 4,  Constructor ("egg",Const 4),   Constructor ("egg",Constructor ("egg",Const 4)),    Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))] ,Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],
                          TupleP[ConstP 17, Wildcard, ConstP 4, ConstructorP ("egg",ConstP 4), ConstructorP ("egg",ConstructorP ("egg",ConstP 4)), TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]) = SOME []
val test_match_5 = match (Unit, UnitP) = SOME []

val test_first_match1_1 = first_match1 Unit [UnitP] = SOME []
val test_first_match1_2 = first_match1 (Tuple [Unit, Const 69]) ([TupleP [Variable "Test", ConstP 69]]) = SOME [("Test", Unit)]

val test_first_match_1 = first_match Unit [UnitP] = SOME []
val test_first_match_2 = first_match (Tuple [Unit, Const 69]) ([TupleP [Variable "Test", ConstP 69]]) = SOME [("Test", Unit)]
val test_first_match_3 = first_match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]]) ([ConstP 17,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]]) = SOME []
val test_first_match_4 = first_match (Constructor ("egg",Const 4)) ([ConstP 4]) = NONE


(*
match: Called match on input: (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]), should have gotten: SOME([]) but your function returned otherwise. [incorrect answer]
first_match: Called first_match on input: (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],[ConstP 17,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]]), should have gotten: SOME([]) but your function returned otherwise. [incorrect answer]
prob13 tests failed to run (most likely caused by an incorrect function signature in the submission)
prob2to4 tests failed to run (most likely caused by an incorrect function signature in the submission)
all_answers: Called all_answers on: [the,walrus,and,the,carpenter,talked,of,many,things,,of,shoes,and,ships,and,ceiling,wax,,of,Cabbages,and,Kings.], should have gotten: NONE but your function returned something else [incorrect answer]
all_answers: Called all_answers on: [this,list,has,no,capital,letters], should have gotten: NONE but your function returned something else [incorrect answer]
all_answers: Called all_answers on: [Alabama,Alaska,Arizona,Arkansas,California,Colorado,Connecticut,Delaware,Florida,Georgia,Hawaii,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,Maine,Maryland,massachusetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New Hampshire,New Jersey,New Mexico,New York,NorthCarolina,North Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode Island,southCarolina,South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,Washington,West Virginia,Wisconsin,Wyoming], should have gotten: NONE but your function returned something else [incorrect answer]
*)