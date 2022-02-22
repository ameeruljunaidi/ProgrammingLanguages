(*
    Date is an int * int * int (year, month date)
    Need to work correctly for reasonable dates, but don't have to check for non-reasonable dates
    A day of year is from 1 to 365 (33 represents Feb 2)
*)

(*
    Write a function is_older
    Takes two dates and evaluates to true of false
    True if the frist argument is a date that comes before the second argument
    False if they're the same dates
*)
fun is_older(first: int * int * int, second: int * int * int) = 
    if #1 first < #1 second
    then true
    else if #1 first = #1 second andalso #2 first < #2 second
    then true
    else if #2 first = #2 second andalso #3 first < #3 second
    then true
    else false

(*
    Write function number_in_month
    Takes a list of dates and a month
    Returns how many dates in the list are in the given month
*)
fun number_in_month(dates: int list, month: int) =
    if null dates
    then 0
    else
        let
            val thirtyDays = month = 4 orelse month =  6 orelse month = 9 orelse month =  11
            val thirtyOneDays = month = 1 orelse month = 3 orelse month = 6 orelse month = 7 orelse month = 8 orelse month = 10 orelse month = 12
            val twentyEightDays = month = 2
        in
            if hd dates < 29
            then 1 + number_in_month(tl dates, month)
            else if hd dates > 28 andalso twentyEightDays
            then 0 + number_in_month(tl dates, month)
            else if hd dates > 28 andalso hd dates < 31 andalso thirtyDays
            then 1 + number_in_month(tl dates, month)
            else if hd dates > 28 andalso hd dates < 32 andalso thirtyOneDays
            then 1 + number_in_month(tl dates, month)
            else 0
        end

val ans = number_in_month([1, 28, 29, 31], 4)