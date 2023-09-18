let rec sum (m, n) =
    match n with
    | 0 -> m
    | n -> m + n + sum (m, n - 1)
