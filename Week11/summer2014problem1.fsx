let rec f n = function 
    | 0 -> 1
    | k when k>0 -> n * (f n (k-1))
    | _ -> failwith "illegal argument";;

let rec g p f = function
    | [] -> []
    | x::xs when p x -> f x :: g p f xs
    | _::xs -> g p f xs;;

// type T = 
//     | A of int
//     | B of string
//     | C of T*T;;

// let rec h = function
//     | A n -> string n
//     | B s -> s
//     | C(t1,t2) -> h t1 + h t2;;
// let sq = Seq.initInfinite (fun i -> 3*i);;
// let k j = seq {for i in sq do
// yield (i,i-j) };;
// let xs = Seq.toList (Seq.take 4 sq);;
// let ys = Seq.toList (Seq.take 4 (k 2));;

//Problem 1

// f 0 0 = 1
// f 5 2 = 5 * f(1) -> 5 * 5 * f(0) -> 5 * 5 * 1

//Exponent, first value is the number, and the second value is the exponent

//Typing: int -> int -> int


//g p f(0 0) [5; 5] = f(0 0 ) :: fss

//typing: (int -> bool) * (int -> int -> int) -> int lst -> int list
//Goes through a list of integers. If the predicate p is true, then it takes the element, and computes it via f and adds it to the list. if p is false, it goes to the next value

let rec f2 n k m = 
    match k with
    | 0 -> 1 * m
    | k when k>0 -> f2 n (k-1) (n * m)
    | _ -> failwith "illegal argument";;

f2 2 2 1;;

let f3 n = 
    let rec aux acc k =
        match k with
        | 0 -> acc
        | k when k > 0 -> aux (acc * n) (k - 1)
        | _ -> failwith "illegal argument"
    aux 1

let sq = Seq.initInfinite (fun i -> 3*i);
//4

//sq Type: Seq<int>

//k Type: Seq<int*int>
//k takes the sq sequence and creates a new sequencve with an int tuple, where the first value is the normal value from sq and the second value is i subtracted with j

let k j = seq {for i in sq do yield (i,i-j) };;

let test20 = k 2

//xs = 0 3 6 9
//xy = (0,-2)