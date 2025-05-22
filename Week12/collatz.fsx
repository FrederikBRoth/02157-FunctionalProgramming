//1

let nat = Seq.initInfinite (fun i -> i)


let collatz (n: int) : seq<int> =
    if n <= 0 then invalidArg "n" "Input must be positive"
    else
        Seq.unfold (fun current ->
            Some(current, 
                if current = 0 then 1  // From examples: after 1 comes 4
                else if current % 2 = 0 then current / 2
                else 3 * current + 1
            )
        ) n

let collatzSequences : seq<seq<int>> =
    seq { for n in 1 .. 4 -> collatz n }

Seq.toList (Seq.take 8 (collatz 3));;

let stoppingTime =
    Seq.initInfinite (fun i -> Seq.findIndex ( fun t -> t = 1) (collatz (i+1)))

Seq.toList (Seq.take 4 (stoppingTime))

let maxStoppingTimes =
    stoppingTime |> Seq.scan max 0

maxStoppingTimes |> Seq.take 4 |> Seq.toList