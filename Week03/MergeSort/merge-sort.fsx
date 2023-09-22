let randomList n range =
    let rand =
        let gen = new System.Random()
        (fun max -> gen.Next(max))

    List.init n (fun _ -> rand range)

let rl1 = randomList 10 20
let rl2 = randomList 10 20
let rl3 = randomList 10 20

let rec merge l1 l2 =
    match (l1, l2) with
    | ([], []) -> []
    | (x, [])
    | ([], x) -> x
    | (x :: l1, y :: l2) ->
        if x > y then
            y :: x :: merge l1 l2
        else
            x :: y :: merge l1 l2

let rec split list =
    match list with
    | [] -> ([], [])
    | [ x ] -> ([ x ], [])
    | x :: y :: rest ->
        let left, right = split rest
        (x :: left, y :: right)

let rec sort list = 
    
merge rl1 rl2
split rl1
