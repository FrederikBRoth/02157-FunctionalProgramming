let randomList n range =
    let rand =
        let gen = new System.Random()
        (fun max -> gen.Next(max))

    List.init n (fun _ -> rand range)

let rl1 = randomList 2000 20000
let rl2 = randomList 10 20
let rl3 = randomList 10 20

let rec merge l1 l2 =
    match (l1, l2) with
    | (x, []) -> l1
    | ([], x) -> l2
    | (x :: l1, y :: l2) ->
        if x <= y then
            x :: merge l1 (y :: l2)
        else
            y :: merge (x :: l1) l2

let rec split list =
    match list with
    | [] -> ([], [])
    | [ x ] -> ([ x ], [])
    | x :: y :: rest ->
        let left, right = split rest
        (x :: left, y :: right)

let rec sort list =

    match list with
    | [] -> []
    | [ x ] -> [ x ]
    | _ ->
        let (left, right) = split list
        let lsort = sort left
        let rsort = sort right
        merge lsort rsort


let m = merge rl1 rl2
let s = split rl1
let ss = sort rl1
