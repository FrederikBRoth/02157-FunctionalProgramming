let rec fib (n) =
    match n with
    | 0
    | 1 -> n
    | n -> fib (n - 1) + fib (n - 2)
