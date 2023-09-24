let rec satisfy list =
    match list with
    | [] -> true
    | [ x ] -> true
    | x :: y :: xs -> if x <= y then satisfy (y :: xs) else false

let rec count list index =
    match list with
    | [] -> 0
    | x :: xs -> if x = index then 1 + count xs index else count xs index

let rec insert list item =
    match list with
    | [] -> []
    | [ x ] -> if x > item then [ item ] @ [ x ] else [ x ] @ [ item ]
    | x :: xs -> if x > item then item :: x :: xs else x :: insert xs item

let rec intersect l1 l2 =
    match l1, l2 with
    | [], _
    | _, [] -> []
    | x :: xs, y :: ys when x = y -> x :: intersect xs ys
    | x :: xs, y :: ys when x < y -> intersect xs l2
    | _, y :: ys -> intersect l1 ys

let rec plus l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | [], _ -> l2
    | _, [] -> l1
    | x :: xs, y :: ys when x = y -> x :: plus xs ys
    | x :: xs, y :: ys when x < y -> x :: plus xs l2
    | x :: xs, y :: ys when x > y -> y :: plus l1 ys

let rec minus l1 l2 =
    match l1, l2 with
    | [], _ -> []
    | _, [] -> l1
    | x :: xs, y :: ys when x = y -> minus xs ys
    | x :: xs, y :: ys when x < y -> x :: minus xs l2
    | x :: xs, y :: ys when x > y -> minus l1 ys



let l1 = [ 2; 6; 1; 3; 9; 4; 7 ]
let l2 = [ 1; 6; 10; 20; 50; 84; 99 ]
