let rec splitAt i xs =
    if i <= 0 then
        ([], xs)
    else
        match xs with
        | [] -> ([], [])
        | x :: tail ->
            let (xs1, xs2) = splitAt (i - 1) tail
            (x :: xs1, xs2)
