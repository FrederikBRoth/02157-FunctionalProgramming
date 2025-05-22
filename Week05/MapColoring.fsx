// Programs to color a map      --- Michael R. Hansen 27-09-2023
// See Section 5.2 in the textbook and slides from Lectures 3 and 4.

type Map<'c>      = ('c * 'c) list
type Color<'c>    = 'c list
type Coloring<'c> = Color<'c> list

let exMap = [("a","b"); ("c","d"); ("d","a")]


// areNb: Map<'c> -> 'c -> 'c -> bool when 'c: equality
let areNb m c1 c2 = List.contains (c1,c2) m 
                    || List.contains (c2,c1) m;;

// canBeExtBy: Map<'c> -> Color<'c> -> 'c -> bool when 'c: equality
let rec canBeExtBy m col c =
  match  col with
  | []       -> true
  | c'::col' -> not (areNb m c' c) && canBeExtBy m col' c;;

// extColoring: Map<'c> -> Coloring<'c> -> 'c -> Coloring<'c> when 'c: equality
let rec extColoring m cols c =
    match cols with
    | []         -> [[c]]
    | col::cols' -> if canBeExtBy m col c
                    then (c::col)::cols'
                    else col::extColoring m cols' c;;

let addElem x ys = if List.contains x ys then ys else x::ys;;

// countries: Map<'c> -> 'c list
let rec countries = function
    | []           -> []
    | (c1,c2)::m -> addElem c1 (addElem c2 (countries m));;

// colCntrs: Map<'c> -> 'c list -> Coloring<'c> 
let rec colCntrs m = function
    | []    -> []
    | c::cs -> extColoring m (colCntrs m cs) c;;

// colMap: Map<'c> -> Coloring<'c> when 'c: equality
let colMap m = colCntrs m (countries m);;

colMap exMap;;

type Country  = A | B | C | D | E | F
type SmallMap = Map<Country>

#r "nuget: FsCheck";;
open FsCheck;;

// A function checking that all contries in m are in countries m
// prop1: SmallMap -> bool
let prop1 (m:SmallMap) = 
   let cs = countries m
   List.forall (fun (c1,c2) ->  List.contains c1 cs && 
                                List.contains c2 cs   ) m;;

let prop2(m:SmallMap) =
    let cs = countries m
    List.forall(fun c -> List.exists(fun (c1, c2) -> c1 = c || c2 = c) m) cs;;

let prop3(m:SmallMap)=
    let cs = countries m
    List.length cs = Set.count (Set.ofList cs)

let prop4(m:SmallMap) =
    let cs = colMap m
    List.forall(fun colours -> List.forall(fun countries -> List.exists(fun (c1, c2) -> c1 = countries || c2 = countries) m) colours) cs
    
let prop5(m:SmallMap) = 
    let cs = colMap m
    List.forall(fun colours -> (List.isEmpty colours) = false) cs

let prop6 (m: SmallMap) =
    let coloring = colMap m
    List.forall (fun col -> 
        List.forall (fun c1 ->
            List.forall (fun c2 ->
                c1 = c2 || not (areNb m c1 c2)
            ) col
        ) col
    ) coloring

let prop7 (m: SmallMap) =
    let cs = countries m
    let coloring = colMap m
    List.forall (fun c ->
        List.exists (fun col -> List.contains c col) coloring
    ) cs

let prop8 (m: SmallMap) =
    let coloring = colMap m
    let allCountries = List.concat coloring
    List.length allCountries = Set.count (Set.ofList allCountries)
    
// properties validated using FsCheck should be monomorphic                        
let _ = Check.Verbose prop8;;

