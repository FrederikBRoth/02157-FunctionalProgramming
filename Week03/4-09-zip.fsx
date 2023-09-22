let l1 = [ 2; 4; 3; 1 ]
let l2 = [ 2; 6; 9; 4 ]
let l3 = [ 2; 6 ]

let rec zip (l1, l2) =
    match (l1, l2) with
    | ([], []) -> []
    | (_, [])
    | ([], _) -> failwith ("Input needs to have the same length")
    | (x :: l1, y :: l2) -> (x, y) :: zip (l1, l2)
