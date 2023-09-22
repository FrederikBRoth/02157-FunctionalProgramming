let rec numberOf x ys =
    match ys with
    | [] -> 0
    | y :: ys -> if y = x then numberOf x ys + 1 else numberOf x ys

let rec positionOfH x index ys =
    match ys with
    | [] -> []
    | y :: ys ->
        if x = y then
            index :: positionOfH x (index + 1) ys
        else
            positionOfH x (index + 1) ys

let positionOf x ys = positionOfH (x, 0, ys)

let rec filterMap p f xs =
    match xs with
    | [] -> []
    | x :: xs -> if p x then f x :: filterMap p f xs else filterMap p f xs

let l1 = [ 2; 6; 4; 1; 10; 23; 9 ]
let p1 = (fun x -> x >= 0)
