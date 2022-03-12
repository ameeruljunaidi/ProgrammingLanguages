fun factorial1 (s) = if s = 0 then 1 else s * factorial1 (s - 1)

fun factorial2 (s) =
    let fun aux (n, acc) = if n = 0 then acc else aux (n - 1, acc * n)
    in aux (s, 1) end

val test_factorial1_1 = factorial1 (5)
val test_factorial2_1 = factorial2 (5)