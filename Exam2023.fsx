let rec numberOf x ys = 
    match ys with
    |number::rest -> if number = x then 1 + numberOf x rest else numberOf x rest
    | [] -> 0

numberOf 2 [0;2;3;3;0;2;4;2;1]

let rec getLength list =
    match list with
    | og::ogrest -> 1 + getLength ogrest 
    | [] -> 0


getLength [2;42;4;2]

let rec positionOf x ys= 
    let rec position x ys length =
        match ys with
        | number::rest -> if number = x then (length - getLength ys) :: position x rest length else position x rest length
        | [] -> []
    position x ys (getLength ys)

positionOf 2 [0;2;3;3;0;2;4;2;1];;

let rec filterMap p f xs =
    match xs with
    | elem::rest -> if p elem then (f elem) :: filterMap p f rest else filterMap p f rest
    | [] -> []

filterMap (fun x -> x>=2) string [0;2;3;3;0;2;4;2;1]

//problem 2
//splitAt -1 [1;2;3]:       Value: ([], [1;2;3])
//splitAt 3 [1;2;3;4;5]:    Value: ()

let rec splitAt i xs =
    if i<=0 then ([],xs)
    else match xs with
            | [] -> ([],[])
            | x::tail -> 
                let (xs1,xs2) = splitAt (i-1) tail 
                (x::xs1,xs2)

splitAt 3 [1;2;3;4;5];;
splitAt 4 [1;2;3];;

//the type is int -> a' list -> (a' list * a' list)

//([x1; ... xi], [xi+1; ...xn])

let caracteristicSeq p s = 
    Seq.map (fun v -> if p v then 1 else 0) s

let fracSeq s =
    Seq.mapi(fun i v -> float (v/i+1)) s

let accSum s = 
    Seq.scan (+) 0 s

//problem 4

type Volume = float
type Piece = A | B | C | Plastic of Volume | Glass of Volume

let pieces = [A; B; C; Plastic(1.0); Glass(0.5)]

let rec isWellformed p =
    match p with
    |Plastic v -> v > 0.0
    |Glass v -> v > 0.0
    | _ -> true

let rec isWellformedList list =
    match list with
    |piece::rest -> if isWellformed piece then isWellformedList rest else false
    | [] -> true

let deposit piece = 
    match piece with
    |Plastic v when v <= 0.5 -> 1.0
    |Plastic v -> 3.0
    |Glass v when v >= 1.0 -> 3.0
    |Glass v -> 1.5
    |A -> 1.0
    |B -> 1.5
    |C -> 3.0

let rec totalDeposit plist =
    match plist with
    |piece::rest -> deposit piece + totalDeposit rest
    | [] -> 0.0

//Type of toSummary: (int * int)
let rec toSummary list =
    List.fold(fun acc piece -> 
        let (r, p) = acc
        match piece with
        |Plastic v -> (r, p+1)
        |Glass v -> (r, p+1)
        |A -> (r+1, p)
        |B -> (r+1, p)
        |C -> (r+1, p)
    ) (0, 0) list

toSummary pieces

//problem 5

type Concept = string;;

type Ontology = O of Concept * Classification
and Classification = Ontology list;;

let o1 = O("Product", [O("DVD", []); O("Book", [O("Science", []); O("Pocket", [])]); O("CD", [])])

let rec occursIn c ontology = 
    match ontology with
    | O(concept, classification) when concept = c -> true
    | O(_, classification) -> occursInList c classification
and occursInList c list =
    match list with
    |elem::rest -> occursIn c elem || occursInList c rest
    | [] -> false

occursIn "Prawdoduct" o1

let rec elementaryConcepts ontology =
    match ontology with
    | O(concept, []) -> [concept]
    | O(_, classi) -> elementaryConceptsList classi
and elementaryConceptsList list = 
    match list with
    | [] -> []
    | elem::rest -> elementaryConcepts elem @ elementaryConceptsList rest

elementaryConcepts o1

let rec find c o = 
    match o with
    | O(concept, classi) -> if concept = c then Some concept else findList c classi
and findList c list = 
    match list with
    | [] -> None
    | elem::rest -> 
        match find c elem with
        | Some x -> Some x
        | None -> findList c rest

find "Science" o1

//Type Concept -> Ontology -> option<Concept>

let rec pathsOf ontology =
    match ontology with
    | O(concept, []) -> [[concept]]  // Leaf: one path
    | O(concept, children) -> 
        List.map (fun path -> concept :: path) (pathsOfList children)

and pathsOfList classification =
    match classification with
    | [] -> []
    | elem :: rest ->
        pathsOf elem @ pathsOfList rest


pathsOf o1