let rec p q =
    function
    | [] -> []
    | x :: xs ->
        let ys = p q xs
        if q x then x :: ys else ys @ [ x ]
