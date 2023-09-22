let rec f (xs, rs) =
    match xs with
    | [] -> rs
    | [ x ] -> x :: rs
    | x1 :: x2 :: xs -> x1 :: f (xs, x2 :: rs)

let g xs = f (xs, [])
