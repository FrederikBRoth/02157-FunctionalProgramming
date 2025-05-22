//Problem 1

//1 skipWhile
//('a -> bool) is what is is due to it being used in the 'when' area. This requires a boolean value, therefor it must return that
//It takes a list as an argument due to how the match is structured. x::xs represents the head of the list and the rest, therefor it takes a list
//the last branch returns an empty list, therefor the return is a list aswell

// 2. Evaluation

let diff5 n = n<>5

//skipWhile diff5 [2;6;5;1;5;6]
//-> skipWhile diff5 2 [6;5;1;5;6]
//-> skipWhile diff5 6 [5;1;5;6]
//-> [5;1;5;6]

//3 SkipWhile ignores every element in a list until the predicate is false. After that it spits out the remained of the list
//  TakeWhile takes every element and puts it into a list untill the predicate is false, after that it exits the program

//Skipwhile is not tail recursive, but i don't think it is required as nothing is accumulated
//takeWhile is not tail recursive, here is a version of it
let rec takeWhile p = function
    | x::xs when p x -> x::takeWhile p xs
    | _ -> [];;
let rec takeWhileR p acc = function
    | x::xs when p x -> takeWhileR p (acc@[x]) xs
    | _ -> acc;;


takeWhileR diff5 [] [2;6;5;1;5;6];;


type T = Leaf of char | Branch of T*T

let t0 = Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c')))
let t1 = Leaf('a')

let rec toList tree = 
    match tree with
    | Leaf l -> [l]
    | Branch (b1, b2) -> toList b1 @ toList (b2)

toList t0

let rec legal t = 
    let list = toList t
    let listSet = Set.ofList list
    listSet.Count = list.Length && list.Length >= 2

legal t1

type Dir = | L // go left
           | R // go right
type Code = Dir list
type CodingTable = Map<char, Code>

let codingTable : CodingTable = 
    Map.ofList [
        ('a', [L])
        ('b', [R; L])
        ('c', [R;R])
    ]
exception CharNotFoundException
let rec encode ct clist = 
    List.fold(fun acc char -> 
        match Map.tryFind char ct with
        | Some value -> acc @ value
        | None -> raise CharNotFoundException
    ) [] clist

let code = encode codingTable (toList t0)

let ofT (tree: T) : CodingTable =
    let rec build path t =
        match t with
        | Leaf c -> Map.ofList [(c, path)]
        | Branch (left, right) ->
            let leftMap = build (path @ [L]) left
            let rightMap = build (path @ [R]) right
            Map.ofList (
                Map.toList leftMap @ Map.toList rightMap
            )
    build [] tree


ofT t0

let rec firstCharOf t code = 
    match t, code with
    | Leaf c, rest -> (c, rest) // done
    | Branch (left, _), L::cs -> firstCharOf left cs
    | Branch (_, right), R::cs -> firstCharOf right cs

let rec decode t code = 
    if List.isEmpty code then
        []
    else
        let (ch, rest) = firstCharOf t code
        ch :: decode t rest

decode t0 code