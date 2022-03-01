val test_all_except_option_1 = all_except_option ("i", ["i"]) = SOME []
val test_all_except_option_2 = all_except_option ("i", ["am"]) = NONE
val test_all_except_option_3 = all_except_option ("i", ["i", "am", "iron", "man"]) = SOME ["am", "iron", "man"]
val test_all_except_option_4 = all_except_option ("am", ["i", "am", "iron", "man"]) = SOME ["i", "iron", "man"]
val test_all_except_option_5 = all_except_option ("ant", ["black", "ant", "iron"]) = SOME ["black", "iron"]


val test_get_substitutions1_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test_get_substitutions1_2 = get_substitutions1 ([["i", "am", "iron"],["black", "widow", "iron"]], "iron") 
                              = ["i", "am", "black", "widow"]
val test_get_substitutions1_3 = get_substitutions1 ([["i", "am", "iron"],["black", "widow", "iron"]], "hawk") = []
val test_get_substitutions1_4 = get_substitutions1 ([["i", "am", "iron"],["ant", "widow", "iron"]], "widow") 
                              = ["ant", "iron"]
val test_get_substitutions1_5 = get_substitutions1 ([["i"],["ant", "widow"], ["iron", "ant"]], "ant") 
                              = ["widow", "iron"]

val test_get_substitutions2_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test_get_substitutions2_2 = get_substitutions2 ([["i", "am", "iron"],["black", "widow", "iron"]], "iron") 
                              = ["i", "am", "black", "widow"]
val test_get_substitutions2_3 = get_substitutions2 ([["i", "am", "iron"],["black", "widow", "iron"]], "hawk") = []
val test_get_substitutions2_4 = get_substitutions2 ([["i", "am", "iron"],["ant", "widow", "iron"]], "widow") 
                              = ["ant", "iron"]
val test_get_substitutions2_5 = get_substitutions2 ([["i"],["ant", "widow"], ["iron", "ant"]], "ant") = ["widow", "iron"]

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

val test_card_color_1 = card_color (Clubs, Num 2) = Black
val test_card_color_2 = card_color (Spades, Jack) = Black
val test_card_color_3 = card_color (Diamonds, King) = Red
val test_card_color_4 = card_color (Hearts, Jack) = Red
val test_card_color_5 = card_color (Clubs, Queen) = Black

val test_card_value_1 = card_value (Clubs, Num 2) = 2
val test_card_value_2 = card_value (Spades, Jack) = 10
val test_card_value_3 = card_value (Diamonds, Queen) = 10
val test_card_value_4 = card_value (Clubs, King) = 10
val test_card_value_5 = card_value (Clubs, Ace) = 11

val test_remove_card_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test_remove_card_2 = remove_card ([(Hearts, Ace), (Diamonds, Ace)], (Hearts, Ace), IllegalMove) = [(Diamonds, Ace)]
val test_remove_card_3 = ((remove_card ([(Hearts, Ace), (Diamonds, Ace)], (Spades, Ace), IllegalMove); false) handle IllegalMove => true)

val test_all_same_color_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test_all_same_color_2 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Hearts, Ace), (Hearts, Ace)] = true
val test_all_same_color_3 = all_same_color [(Clubs, Ace), (Hearts, Ace)] = false
val test_all_same_color_4 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Ace), (Hearts, Ace)] = false
val test_all_same_color_5 = all_same_color [(Hearts, Ace), (Diamonds, Ace)] = true
val test_all_same_color_6 = all_same_color [(Clubs,Ace),(Spades,Ace),(Diamonds,Ace)] = false

val test_card_sum_1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test_card_sum_2 = sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13
val test_card_sum_3 = sum_cards [(Clubs, Num 2),(Clubs, King)] = 12
val test_card_sum_4 = sum_cards [(Clubs, Num 2),(Clubs, Queen), (Clubs, Queen)] = 22
val test_card_sum_5 = sum_cards [(Clubs, Num 2),(Clubs, Num 9)] = 11
val test_card_sum_6 = sum_cards [(Clubs, Num 2),(Clubs, Jack)] = 12

val test_score = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test_officiate_1 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3

val test_officiate_2 = ((officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42); false) handle IllegalMove => true)

(* First submission errors:
all_same_color: Your function returns an incorrect result when given the list [(Clubs,Ace),(Spades,Ace),(Diamonds,Ace)] [incorrect answer]
officiate: the game ends with no more move and the held-cards are of different colors. [incorrect answer]
3a tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
3b tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
Used '#' 3 times.
*)