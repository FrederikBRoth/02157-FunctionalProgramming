type Poly = int list

let rec add (p1: Poly, p2: Poly) : Poly =
    match (p1, p2) with
    | ([], _) -> p2
    | (_, []) -> p1
    | (x :: p1, y :: p2) -> x + y :: add (p1, p2)

let rec mulC (c: int, p: Poly) : Poly =
    match p with
    | [] -> p
    | x :: p -> x * c :: mulC (c, p)

let rec sub (p1: Poly, p2: Poly) : Poly =
    match (p1, p2) with
    | ([], []) -> []
    | ([], y :: p2) -> -y :: sub (p1, p2)
    | (x :: p1, []) -> x :: sub (p1, p2)
    | (x :: p1, y :: p2) -> x + -y :: sub (p1, p2)

let rec mulX (p1: Poly) : Poly = 0 :: p1

let rec mul (p1: Poly, p2: Poly) : Poly =
    match (p1, p2) with
    | ([], _)
    | (_, []) -> []
    | (x :: p1, _) -> add (mulC (x, p2), mulX (mul (p1, p2)))

let rec eval (x: int, p1: Poly) : int =
    match p1 with
    | [] -> 0
    | a :: p1 -> a + x * eval (x, p1)

// Week 02

let rec fixList (list: int list) : Poly =
    match list with
    | 0 :: l -> fixList (l)
    | fixedList -> List.rev fixedList

let rec isLegal (list: int list) : bool =
    match list with
    | [] -> true
    | x :: list -> if x = 0 then false else isLegal (list)

let ofList (list: int list) : Poly =
    if isLegal (list) then list else fixList (List.rev list)

let toString (ns: int list) =
    String.concat " + " (List.mapi (fun i n -> string n + "x^" + string i) ns)

let removeA (p: Poly) : Poly =
    match p with
    | [] -> []
    | _ :: rest -> rest

let derivative (p: Poly) : Poly =
    let derive = ofList (List.mapi (fun i n -> n * i) p)

    removeA (derive)


let rec polyPowH (p: Poly, pg: Poly, count: int) =
    match count with
    | 0 -> [ 1 ]
    | 1 -> p
    | count -> polyPowH (mul (p, pg), pg, count - 1)

let polyPow (p: Poly, count: int) = polyPowH (p, p, count)

let rec compositeH (p1: Poly, p2: Poly, count: int) =
    match p1 with
    | [] -> []
    | x :: rest -> add (mulC (x, polyPow (p2, count)), compositeH (rest, p2, count + 1))
// Part 3
// The Poly type we have made is an invariant of the list<int> type. This means that there are a number of rules
// created that we must follow, such as there may be no trailing '0' in the list etc.
//
// This needs to be verified in the functions by either calling the isLegal function before, or making the list into a Poly using the
// OfList function
let composite (p1: Poly, p2: Poly) : Poly = compositeH (p1, p2, 0)
let p1: Poly = [ 2; 3; 5 ]
let p2: Poly = [ 5; -2; 2; 10; -4 ]
let p3: Poly = [ -3; 2; 7; -2 ]
let p4: Poly = [ 2 ]
let pc1: Poly = [ 2; 0; 0; 4 ]
let pc2: Poly = [ 0; 3; 2 ]
let fakep1: int list = [ 2; 5; 0; 0; 0 ]
let fakep2: int list = [ 2; 0; 1; 0; 1; 0 ]
