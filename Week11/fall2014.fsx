//Problem 2

//4
let rec f i = function
    | [] -> []
    | x::xs -> (x+i)::f (i*i) xs;;

//Typing: int -> int list -> int list

//Takes a list of ints and adds the exponent 2 of i to each element for each revolution

let rec fT i acc = function
    | [] -> acc
    | x::xs -> fT (i*i)  (acc@[x+i]) xs

let rec fC i cont = function
    | [] -> cont []
    | x::xs -> fC (i*i)  (fun res -> cont (x+i::res)) xs

let test = [2;5;10;2]

f 2 test;;

fT 2 []test;;

fC 2 id test

let multTable i = 
    Seq.init (10) (fun current -> (current+1)*i);;

Seq.toList(Seq.take 10(multTable 3))



let tableOf m n f : seq<int * int * 'a> =
    seq {
        for i in 1 .. m do
            for j in 1 .. n do
                yield (i, j, f i j)
    }

let infiniteAs : seq<string> =
    Seq.unfold(fun s -> Some(s, s + "a")) "a"

infiniteAs |> Seq.take 5 |> Seq.toList