let rec pow (s: string, n: int) =
    match n with
    | 0 -> ""
    | n -> s + pow (s, n - 1)
