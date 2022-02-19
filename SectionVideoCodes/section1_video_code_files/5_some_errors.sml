(* Programming Languages, Dan Grossman *)
(* Section 1: Some Errors *)

(* This program has several errors in it so we can try to debug them. *)

val x = 34;

val y = x + 1; (*this is a sytax error*)

val z = if y > 0 then false else x < 4; (*y is type bool not int*)

val q = if y > 0 then 0 else 42; (* can't leave the else *)

val a = ~5; (*in ML you can only use (-) for binary operation, use the (~) operator in this case*)

val w = 0;

val funny = 34; (*fun means something in ml*)

val v = x div (w + 1); (*that is how you do division with float, you have to spell div for division*)

val fourteen = 7 + 7;
