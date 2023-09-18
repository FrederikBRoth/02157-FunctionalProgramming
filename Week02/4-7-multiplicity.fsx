let rec multip (n: int, xs) =
    match xs with
    | [] -> 0
    | x :: xs -> if x = n then multip (n, xs) + 1 else multip (n, xs)
