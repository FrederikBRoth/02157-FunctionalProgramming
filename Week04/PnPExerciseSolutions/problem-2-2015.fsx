let rec mixMap f xs ys =
    match (xs, ys) with
    | ([], []) -> []
    | (x :: xs, y :: ys) -> f (x, y) :: mixMap f xs ys


let rec unmixMap f g xs =
    match xs with
    | [] -> ([], [])
    | x :: xs ->
        let (left, right) = x
        let (flist, glist) = unmixMap f g xs
        (f left :: flist, g right :: glist)

let l1 = [ 2; 5; 1; 7; 2; 3; 5 ]
let l2 = [ 1; 7; 2; 1; 2; 5; 1 ]

let f = (fun (y, x) -> (y + 2, x + 2))
let g = (fun y -> y - 2)
