(* Coursera Programming Languages, Homework 3, Provided Code *)

(**** for the challenge problem only ****)

datatype typ = Anything
       | UnitT
       | IntT
       | TupleT of typ list
       | Datatype of string

(**** you can put all your code here ****)

fun only_capitals ss = List.filter (fn s => Char.isUpper (String.sub (s,0))) ss

fun longest_string1 ss = List.foldl (fn (s,x) => if String.size s > String.size x then s else x) "" ss

fun longest_string2 ss = List.foldl (fn (x,s) => if String.size s > String.size x then s else x) "" ss

fun longest_string_helper f ss = List.foldl (f) "" ss

val longest_string3 = longest_string_helper (fn (s,x) => if String.size s > String.size x then s else x)

val longest_string4 = longest_string_helper (fn (x,s) => if String.size s > String.size x then s else x)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

exception NoAnswer

fun first_answer f xs =
  case xs of
     [] => raise NoAnswer
   | x::xs' => case f x of
                  NONE => first_answer f xs'
                | SOME y => y

fun all_answers f xs =
  case xs of
     [] => SOME []
   | _ => let fun aux (ys, acc) =
              case ys of
                 [] => acc
               | y::ys' => case f y of
                              NONE => aux (ys', acc)
                            | SOME w => aux (ys', w @ acc)
          in case aux (xs, []) of
                [] => NONE
              | v => SOME v
          end


datatype pattern = Wildcard
       | Variable of string
       | UnitP
       | ConstP of int
       | TupleP of pattern list
       | ConstructorP of string * pattern

datatype valu = Const of int
       | Unit
       | Tuple of valu list
       | Constructor of string * valu

fun g f1 f2 p =
  let val r = g f1 f2 
  in
    case p of
       Wildcard          => f1 ()
     | Variable x        => f2 x
     | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
     | ConstructorP(_,p) => r p
     | _                 => 0
  end

fun count_wildcards_no_g p =
  case p of
     ConstructorP (_,p') => count_wildcards_no_g (p')
   | TupleP ps => (case ps of
                     [] => 0
                   | x::xs' => (count_wildcards_no_g x) + (count_wildcards_no_g (TupleP xs')))
   | Wildcard => 1
   | _ => 0

fun count_wildcards p = 

val test_count_wildcards_1 = count_wildcards Wildcard = 1