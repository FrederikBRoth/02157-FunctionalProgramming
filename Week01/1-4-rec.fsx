let rec incr (n) =
    match n with
    | 1 -> 1
    | n -> n + incr (n - 1)
