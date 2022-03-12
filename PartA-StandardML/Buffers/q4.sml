(*
   T1 -> T2
   T1 is T3 T4 T5 T6
   T3 is T7 -> T8

   T4 is int
   T5 is int
   T6 is T9 list


*)
fun null xs = xs = []

val ans = null []

fun test xs =
  case xs of
     [] => 0
   | x::xs' => case x of
      (a, _) => a + test xs'

val ans1 = test [(1, 2), (3, 4), (5, 6)]



fun null xs = case xs of [] => true | _ => false