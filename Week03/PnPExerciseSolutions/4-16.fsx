let l1 = (4, [ 5; 2; 1; 0 ])
let l2 = [ (2, 5); (3, 6) ]
let l3 = [ 5; 1; 7; 9 ]

let rec f =
    function
    | (x, []) -> []
    | (x, y :: ys) -> (x + y) :: f (x - 1, ys)

let rec g =
    function
    | [] -> []
    | (x, y) :: s -> (x, y) :: (y, x) :: g s

let rec h =
    function
    | [] -> []
    | x :: xs -> x :: (h xs) @ [ x ]
