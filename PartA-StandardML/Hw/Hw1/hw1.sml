(*
    Date is an int * int * int (year, month date)
    Need to work correctly for reasonable dates, but don't have to check for non-reasonable dates
    A day of year is from 1 to 365 (33 represents Feb 2)
*)

(*
    @param f firdate date
    @param s second date
    @return true if firt date is older
*)
fun is_older (f: int * int * int, s: int * int * int) : bool = 
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
fun number_in_month (ds: (int * int * int) list, m: int) : int  =
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
fun number_in_months (ds: (int * int * int) list, ms: int list) : int =
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
fun dates_in_month (ds: (int * int * int) list, m: int) : (int * int * int) list = 
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
fun dates_in_months (ds: (int * int * int) list, ms: int list) : (int * int * int) list =
    if null ms
    then []
    else dates_in_month (ds, hd ms) @ dates_in_months (ds, tl ms)


(*
    @param ss list of strings
    @param n and int n to index
    @return the nth element of the list where head of the list is 1st
*)
fun get_nth (ss: string list, n: int) : string =
    if null ss orelse n < 1
    then ""
    else if n <> 1
    then get_nth (tl ss, n - 1)
    else hd ss

(*
    @param the date
    @return return a strin gof the from January 20, 2013, for example
*)
fun date_to_string (d: int * int * int) : string =
    let
      val months = ["January", "Februrary", "March", "April", "May", "June", "July", "August", "September", "October",
                    "November", "December"]
      val some_month = get_nth (months, #2 d)
    in
      some_month ^ " " ^ Int.toString (#3 d) ^ ", " ^ Int.toString (#1 d)
    end

(*
    @param sum the sum we are looking for
    @param xs the list of integers 
    @return an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of 
    the list add to sum or more. Assume the entire list sums to more than the passed invalue; it is okay for an 
    exception to occur if this is not the case
*)
fun number_before_reaching_sum (sum: int, xs: int list) : int = 
    if null xs
    then 0
    else if sum - hd xs <= 0
    then 0
    else 1 + number_before_reaching_sum (sum - hd xs, tl xs)

(*
    @param d the day of the year between 1 to 365
    @return an int of the month
*)
fun what_month (d: int) : int =
    number_before_reaching_sum (d, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

(*
    @param d1 the first day
    @param d2 the second day
    @return list of days, the result will have length day2 - day1 + 1 or length 0 if day1 > day2
*)
fun month_range (d1 : int, d2: int) : int list = 
    if d1 > d2
    then [] 
    else what_month (d1) :: month_range (d1 + 1, d2)

(*
    @param ds list of dates
    @return option of the oldest date
*)
fun oldest (ds: (int * int * int) list) : (int * int * int) option = 
    if null ds
    then NONE
    else if null (tl ds)
    then SOME (hd ds)
    else
        let
          val other_date = oldest (tl ds)
        in
          if is_older (hd ds, valOf other_date)
          then SOME (hd ds)
          else other_date
        end