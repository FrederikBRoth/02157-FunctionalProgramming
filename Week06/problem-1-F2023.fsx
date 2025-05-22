type Multiset<'a when 'a : equality> = ('a * int) list;;
let ms = [("b", 3); ("a", 5); ("d", 1)]
let ms2 = [("b", 1); ("a", 10); ("e", 1)]


let inv (ms: Multiset<'a>) =
    // Extract the elements and multiplicities
    let elements = List.map fst ms
    let multiplicities = List.map snd ms

    // Check that all multiplicities are positive
    let allPositive = List.forall (fun n -> n > 0) multiplicities

    // Check that all elements are distinct
    let noDuplicates = 
        let uniqueElements = Set.ofList elements
        List.length elements = Set.count uniqueElements

    // Both conditions must be true
    allPositive && noDuplicates

inv ms

let insert elem count  ms = 
    if not (List.exists(fun (a, c) -> a = elem) ms) then
        (elem, count) :: ms
    else
        List.fold(fun acc (e, c) -> if e = elem then (e, c + count) :: acc else (e, c) :: acc ) [] ms



let numberOf e ms = 
    let (el, count) = List.find(fun (a, c) -> a = e) ms
    count

let delete e ms = 
    List.filter(fun (a, c) -> not (a = e)) ms

let union ms1 ms2 =
    let newms = List.foldBack(fun (ms1e, ms1c) acc -> insert ms1e ms1c acc) ms1 []
    let newms = List.fold(fun acc (ms1e, ms1c)  -> insert ms1e ms1c acc) newms ms2 
    newms
    
let test = insert"c" 5 ms;

let test2 = numberOf "c" test

let test3 = delete "c" ms

let test4 = union ms ms2
