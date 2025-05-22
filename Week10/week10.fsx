//Problem 1

let rec ins x = function
    | (y,n) :: ys when x=y -> (y,n+1)::ys
    | pair :: ys -> pair::ins x ys
    | [] -> [(x,1)];

let rec cntBy f xs acc = 
    match xs with
    | [] -> acc
    | x::rest -> cntBy f rest (ins (f x) acc);;

let countBy f xs = cntBy f xs [];;


// countBy f [1;2;3]
// ⇝ countBy f [1;2;3] []
// ⇝ countBy f [2;3] (ins 1 [])
// ⇝ countBy f [2;3] [(1,1)]
// ⇝ countBy f [3] (ins 0 [(1,1)])
// ⇝ countBy f [3] [(1,1);(0,1)]
// ⇝ countBy f [] (ins 1 [(1,1);(0,1)])
// ⇝ countBy f [] [(1,2);(0,1)]
// ⇝ [(1,2);(0,1)]

//Problem 2

type T = | One of int | Two of int * T * int * T

let rec f p t =
    match t with
    | One v when p v -> [v] (* C1 *)
    | Two(v1,t1,_,_) when p v1 -> v1::f p t1 (* C2 *)
    | Two(_,_,v2,t2) -> v2::f p t2 (* C3 *)
    | _ -> [] (* C4 *) 

//p v is some sort of check that has the typing int -> bool

//The overall computation is that it traverses through a tree. It only goes down the left node if the p check is valid but always checks the right nodes. 

//Therefor it goes through almost every node in the tree and checks for a spefici check. if check is true, adds it to a list and returns it

// the tests

type Trie<'a> = N of 'a * bool * Children<'a>
    and Children<'a> = Trie<'a> list

let t1 = N(0, false, [N(0, false, [N(1,true,[])])]);;
let t2 = N(0, true, [N(0, false, [N(1,true,[])])]);;
let ta = N(1,true,[N(2,true,[])]);;
let tb = N(3,false,[N(0,true,[])]);;
let tc = N(2,false,[]);;
let t3 = N(0,false, [ta;tb;tc]);;

let rec trieNodeCount (t: Trie<'a>) : int =
    let (N(_, _, children)) = t
    1 + List.sumBy trieNodeCount children

let rec accept (w: 'a list) (N(v, isFinal, children): Trie<'a>) : bool =
    match w with
    | [] -> isFinal
    | x :: xs ->
        // Find a matching child node with value x
        let matches = List.filter (fun (N(vChild, _, _)) -> vChild = x) children
        // Recurse into any matching child  
        List.exists (fun child -> accept xs child) matches

trieNodeCount t3

accept [0;3] t3

let rec wordsOf (N(v, isFinal, children): Trie<'a>) : Set<'a list> =
    let childWords = Set.unionMany (List.map wordsOf children) 
        
           // recursively get sets from each child
             // merge all child sets

    let withPrefix =
        childWords |> Set.map (fun w -> v :: w)

    if isFinal then
        Set.add [v] withPrefix
    else
        withPrefix
wordsOf t3

let rec hasUseless (N(v, isFinal, children): Trie<'a>) = 
    match children with
    | [] -> not isFinal  // if no children and not final → useless leaf
    | _ ->
        // Recursively check children
        List.exists hasUseless children

hasUseless t3

let rec degree (N(_, _, children) : Trie<'a>) : int =
    let childDegrees = List.map degree children
    let maxChild = if childDegrees = [] then 0 else List.max childDegrees
    max (List.length children) maxChild

degree t3