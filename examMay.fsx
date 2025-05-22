type Price = float
type Item = string
type PricedItem = Item * Price

type Register = PricedItem list

let reg = (["apple", -1.0; "banana", 2.0; "orange", 3.0])
let rec pricesOK (reg: Register)  = 
    match reg with
    | [] -> true
    | (i, p)::xs -> if p < 0 then false else pricesOK xs

let rec getPrice (item: Item) (reg: Register): Price = 
    match reg with
    | [] -> raise (System.Exception("Item not found"))
    | (i, p)::xs -> if i = item then p else getPrice item xs

let rec f2 xs ys =
    match xs with
    | [] -> ys
    | x::rest -> f2 rest x::ys;;

type T = A of int | B of T * T

let rec f3 t = match t with
    | A i -> i
    | B(t1,t2) -> f3 t1 + f3 t2;;