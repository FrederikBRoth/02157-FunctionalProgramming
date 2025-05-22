// 1. 
// We have a tuple with three elements 
// The type of the different elements of the tuple is as follows.
// Number: string
// YearofBirth: int
// Description of interest: list<string>
// Description: string * int * list<string>
// Register: list<string * int * list<string>>
// Arangement: string * int * list<string> -> bool

// type Description = {Number: string; YearOfBirth: int; Themes: string list}
let reg= [("number", 1990, ["fuck"]); ("number2", 1982, ["soccer"; "jazz"]); ("number3", 1992, ["soccer"; "jazz"])]

let rec contains item list =
    match list with
    | [] -> false
    | x :: xs -> if x = item then true else contains item xs

let p1 (no: string, yb: int, ths: string list) =
    yb > 1982 && contains "soccer" ths && contains "jazz" ths

let p2 (no: string, yb: int, ths: string list) =
    yb > 1982 && contains "soccer" ths || contains "jazz" ths

let rec extractTargetGroup (p : (string * int * string list -> bool), r : (string * int * string list) list) = 
    match r with
    | [] -> []
    | (no, yb, ths) :: rest -> 
    if p (no, yb, ths) 
    then (no, yb) :: extractTargetGroup( p, rest) 
    else extractTargetGroup( p, rest)


let result = extractTargetGroup(p2, reg)