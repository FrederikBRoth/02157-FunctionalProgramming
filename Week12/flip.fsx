let flip (s: seq<'a * 'b>) : seq<'b * 'a> =
    Seq.map (fun (a, b) -> (b, a)) s

let input = seq [("a", 1); ("b", 2); ("c", 3)]
let flipped = flip input |> Seq.toList
// Output: [(1, "a"); (2, "b"); (3, "c")]

let dia n : seq<int * int> =
    Seq.init (n + 1) (fun i -> (i, n - i))

dia 0 |> Seq.toList;;   // [(0, 0)]
dia 2 |> Seq.toList;;   // [(0, 2); (1, 1); (2, 0)]
dia 3 |> Seq.toList;;   // [(0, 3); (1, 2); (2, 1); (3, 0)]

let allCoordinates : seq<int * int> =
    Seq.initInfinite (fun n ->
        if n % 2 = 0 then dia n           // Even n: use dia(n)
        else flip (dia n)               // Odd n: use flipped dia(n)
    )
    |> Seq.collect id   

allCoordinates |> Seq.take 10 |> Seq.toList