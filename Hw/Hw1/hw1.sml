(*
    Date is an int * int * int (year, month date)
    Need to work correctly for reasonable dates, but don't have to check for non-reasonable dates
    A day of year is from 1 to 365 (33 represents Feb 2)
*)

(*
    @param f firdate date
    @param s second date
*)
fun is_older (f: int * int * int, s: int * int * int) = 
    if #1 f < #1 s
    then true
    else if #1 f = #1 s andalso #2 f < #2 s
    then true
    else if #2 f = #2 s andalso #3 f < #3 s
    then true
    else false

(*
    @param ds the list of dates
    @param m the month
    @return the number of dates in the list are in the given month
*)
fun number_in_month (ds: (int * int * int) list, m: int) =
    if null ds
    then 0
    else if #2 (hd ds) = m
    then 1 + number_in_month (tl ds, m)
    else 0 + number_in_month (tl ds, m)


(*
    @param ds list of dates
    @param ms list of months
    @return the number of dates in the list of dates that are in any of the months
*)
fun number_in_months (ds: (int * int * int) list, ms: int list) =
    if null ms
    then 0
    else
        let
          val date_count = number_in_month (ds, hd ms)
        in
          date_count + number_in_months (ds, tl ms)
        end

(*
    @param ds list of dates
    @param m month
    @return a list holding the dates from the argument list of dates that are in the month
    Return list should contain dates in the order they were originally given
*)
fun dates_in_month (ds: (int * int * int) list, m: int) = 
    if null ds
    then []
    else if #2 (hd ds) = m
    then (hd ds) :: dates_in_month (tl ds, m)
    else dates_in_month (tl ds , m)

(*
    @param ds list of dates
    @param ms list of months
    @return a list holding the dats from the argument list of dates that are in any ms
*)
fun dates_in_months (ds: (int * int * int) list, ms: int list) =
    if null ms
    then []
    else dates_in_month (ds, hd ms) @ dates_in_months (ds, tl ms)

(*
    @param ss list of strings
    @param n and int n to index
    @return the nth element of the list where head of the list is 1st
*)
fun get_nth (ss: string list, n: int) =
    if null ss orelse n < 1
    then ""
    else if n <> 1
    then get_nth (tl ss, n - 1)
    else hd ss

(*
    @param the date
    @return return a strin gof the from January 20, 2013, for example
*)
fun date_to_string (d: int * int * int) =
    let
      val months = ["January", "Februrary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
      val some_month = get_nth (months, #2 d)
    in
      some_month ^ " " ^ Int.toString (#3 d) ^ ", " ^ Int.toString (#1 d)
    end



val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"