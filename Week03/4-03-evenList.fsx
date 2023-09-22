let rec evenN n =
    if n <= 0 then
        []
    else
        let evens = evenN (n - 1)
        (2 * n) :: evens
