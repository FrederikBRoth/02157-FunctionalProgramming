let rec ptriangle (n, k) =
    match (n, k) with
    | (n, k) when n = k -> 1
    | (_, 0) -> 1
    | (n, k) when n > k -> ptriangle (n - 1, k - 1) + ptriangle (n - 1, k)
