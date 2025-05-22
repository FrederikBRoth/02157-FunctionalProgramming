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

let f = (fun (y, x) -> (y + 2, x - 2))
let g = (fun y -> y - 2)
let g2 = (fun y -> y + 2)


//With list functions

let mixMapList f xs ys = 
    List.foldBack2(fun  x y acc -> f(x, y) :: acc)xs ys [] 

let unmixMapList f g xs = 
    let left = List.foldBack(fun (left, right) acc -> f(left) :: acc) xs []
    let right = List.foldBack(fun (left, right) acc -> g(right) :: acc) xs []
    (left, right)


let test2 = mixMap f l1 l2
let test3 = unmixMap g g2 test2

let test = mixMapList f l1 l2
let test5 = unmixMapList g g2 test
