
(* there is multiple calls to bad_max *)
fun bad_max (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else if hd xs > bad_max (tl xs)
    then hd xs
    else bad_max (tl xs);

(* can do memoization to store the answer *)
fun good_max (xs : int list) =
    if null xs            (* if the list is null, this is still wrong *)
    then 0                (* then just return 0 *)
    else if null (tl xs)  (* if the list is almost null *)
    then hd xs            (* then just return the head of xs *)
    else
        (* for style, oculd also use a let-binding for (hd xs) *)
        let 
            val tl_ans = good_max (tl xs); (* store the call to good_max in a variable *)
        in
            if hd xs > tl_ans              (* if the head is larger then the rest *)
            then hd xs                     (* then reutrn the head *)
            else tl_ans                    (* if not then return the stored bad_max *)
        end