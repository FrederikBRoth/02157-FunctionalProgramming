//                   Michael R. Hansen  14-08-2023
module Polynomial

// A toString function that is not a prettyprint
let toString (ns: int list) =
    String.concat " + " (List.mapi (fun i n -> string n + "x^" + string i) ns)

type Poly =
    | P of int list

    override p.ToString() =
        match p with
        | P ns -> toString ns

let rec pHelp =
    function
    | 0 :: rest -> pHelp rest
    | xs -> List.rev xs

let prune xs = pHelp (List.rev xs)

let rec addH p q =
    match (p, q) with
    | ([], _) -> q
    | (_, []) -> p
    | (a :: p', b :: q') -> a + b :: addH p' q'

let add p q = prune (addH p q)

let ofArray p = P(prune (List.ofArray p))
let ofList p = P(prune p)

type Poly with

    static member (+)(P p1, P p2) = P(add p1 p2)
