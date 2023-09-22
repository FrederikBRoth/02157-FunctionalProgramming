let p1 x = x > 0
let p2 x = x % 2 = 0

let rec sum (p: (int -> bool), n: int list) =
    match n with
    | [] -> 0
    | n :: xs -> if p (n) then n + sum (p, xs) else sum (p, xs)

let l1 = [ 1; 2; -3; 4 ]
