let rec split list =
    match list with
    | [] -> ([], [])
    | [ x ] -> ([ x ], [])
    | x :: y :: rest ->
        let left, right = split rest
        (x :: left, y :: right)
