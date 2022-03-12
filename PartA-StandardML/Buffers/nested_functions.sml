(* nested functions can be good sometimes, but not other times *)

fun countup(x : int) = 
    let
        fun count(from : int) =
            if from = x                       (* if from is the last number *)
            then x::[]                        (* cons to to an empty list, essentially creating the list *)
            else from :: count (from + 1)     (* else cons from to the list  *)
    in
        count(1)
    end


val out = countup 7