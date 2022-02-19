(* Programming Languages, Dan Grossman *)
(* Section 1: Examples to Demonstrate Shadowing *)

val a = 10;

val b = a * 2;

val a = 5; (*b is still bound to 20 at this point, this is not an assignment statement*)

(* 
    there is no way in ML to mutate 
    there is also no such thing as referencing another variable
    this is why we shouldn't be reusing use int he same repl
*)

val c = b; (*a -> 5, b -> 20, c -> 20, a maps to 5 is no longer relevant*)

val d = a;

val a = a + 1; (*there is such thing as an assignment*)

(* next line does not type-check, f not in environment *)
(* val g = f - 3 because f has not been evaluated yet *)

val f = a * 2;
