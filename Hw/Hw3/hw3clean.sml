(* Coursera Programming Languages, Homework 3, Provided Code *)

(**** for the challenge problem only ****)

datatype typ = Anything
       | UnitT
       | IntT
       | TupleT of typ list
       | Datatype of string

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

exception NoAnswer

(**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s,0)))

val longest_string1 = List.foldl (fn (s,x) => if String.size s > String.size x then s else x) "" 

val longest_string2 = List.foldl (fn (x,s) => if String.size s > String.size x then s else x) "" 

fun longest_string_helper f = List.foldl (f) ""

val longest_string3 = longest_string_helper (fn (s,x) => if String.size s > String.size x then s else x)

val longest_string4 = longest_string_helper (fn (x,s) => if String.size s > String.size x then s else x)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

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

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)

fun same_string(s1 : string, s2 : string) = s1 = s2

fun count_some_var (s, p) = g (fn _ => 0) (fn x => if same_string (x, s) then 1 else 0) p

fun check_pat p =
  let

    fun get_variable v = 
      case v of
         Wildcard          => []
       | Variable x        => [x]
       | ConstructorP(_,w) => get_variable (w)
       | TupleP xs         => (case xs of [] => [] | x::xs' => get_variable (x) @ get_variable (TupleP xs'))
       | _                 => []

    fun check_repeats m =
       case m of
          [] => true
        | m::ms' => case (List.exists (fn s => s = m) ms') of
                       true => false
                     | false => check_repeats ms'

  in check_repeats (get_variable p)
  end


fun match (v, p) =
  case (v, p) of
     (_, Wildcard)         => SOME []
   | (v, Variable s)       => SOME [(s, v )]
   | (Unit, UnitP)         => SOME []
   | (Const x, ConstP y)   => if x = y then SOME [] else NONE
   | (Tuple vs, TupleP ps) => if List.length vs <> List.length ps then NONE
                              else all_answers match (ListPair.zip (vs, ps)) 
   | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 then match (v, p) else NONE
   | (_, _) => NONE

fun first_match v ps =
  case ps of
     [] => NONE
   | p::ps' => SOME (first_answer (fn (x, y) => match (x, y)) [(v, p)]) handle NoAnswer => (first_match v ps')
