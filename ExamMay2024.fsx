type Currency = | DKK // Danish krone
                | SEK // Swedish krone
type Price = float // Expected to be positive
type Item = string
type PricedItem = Item * Price
type Register = PricedItem list // Items are unique in a register
// Prices in DKK
type Purchase = Item list
type Bill = PricedItem list * Price * Currency
type Rate = float // DKK to SEK
let r = 1.56 // 1 DKK is 1.56 SEK
let reg = ([("Ragusa",22.0); ("Bounty",57.0); ("Licorice",44.0)]);;

exception NoItem

let rec pricesOk reg=
    match reg with
    | (_, price)::rest -> if price > 0 then pricesOk rest else false
    | [] -> true
let rec getPrice item reg =
    match reg with
    | (i, p)::rest when i = item -> p
    | (i, p)::rest -> getPrice item rest
    | [] -> raise NoItem

let rec priceOf itm reg curr rt = 
    let price = getPrice itm reg
    match curr with
    | DKK -> price
    | SEK -> price * rt

priceOf "Bounty" reg SEK r;;

let rec pricedItemsOf pur reg curr rt = 
    match pur with
    | item::rest -> (item, priceOf item reg curr rt) :: pricedItemsOf rest reg curr rt
    | [] -> []

pricedItemsOf ["Bounty"; "Ragusa"; "Bounty"] reg DKK r

let rec totalPrice pur reg curr rt = 
    let pricedItems = pricedItemsOf pur reg curr rt
    match pricedItems, pur with
    |(item, price)::rest, pp::rest2 -> price + totalPrice rest2 reg curr rt
    | [], [] -> 0.0

totalPrice ["Bounty"; "Ragusa"; "Bounty"] reg DKK r

let rec mkBill pur reg curr rt = 
    let pricedItems = pricedItemsOf pur reg curr rt
    let totalPrice = totalPrice pur reg curr rt
    (pricedItems, totalPrice, curr)

mkBill ["Bounty"; "Ragusa"; "Bounty"] reg DKK r

let pricesOkLib reg = 
    List.forall(fun (_, cost) -> cost > 0.0 ) reg


pricesOkLib reg

let pricedItemsOfLib pur reg curr rt = 
    List.foldBack(fun item acc -> (item, priceOf item reg curr rt) :: acc) pur []

pricedItemsOfLib ["Bounty"; "Ragusa"; "Bounty"] reg DKK r

let totalPriceLib pur reg curr rt = 
    let pricedItems = pricedItemsOfLib pur reg curr rt
    List.foldBack2(fun (item, price) purrest acc -> acc + price)pricedItems pur 0.0

totalPriceLib ["Bounty"; "Ragusa"; "Bounty"] reg DKK r


// type Currency2 = string
// type Rate2 = Currency2 * float
// let rec priceOfNew itm reg curr rate = 
//     let price = getPrice itm reg
//     match rate with
//     | (currency, rate) when currency = "DKK" -> price
//     | (currency, rate) -> price * rate


// priceOfNew "Bounty" reg ;;

//New type of rate would be Rate = Currency * float

//problem 2
//'a list -> int -> ('a * int) list
//'a is generic since nothing indicates it is anything else.
// int is int, due to it being incremented by an integer in the match statement

let rec f xs i = 
    match xs with
    | x::rest -> (x,i)::f rest (i+1)
    | [] -> [];;

//It turns the list into a list of tuples where the first element is the orignal element from the list and the second element is an integer that is incremented by 1 at its starting value

//3 Evaluation
//f [3;4;5;6] 10
//(3;10) :: f [4;5;6] 11
//(3;10) :: (4;11) :: f [5;6] 12
//(3;10) :: (4;11) :: (5:12) :: f [6] 13
//(3;10) :: (4;11) :: (5:12) :: (6,13) :: f [] 14
//(3;10) :: (4;11) :: (5:12) :: (6,13) :: []
//[(3,10);(4,11);(5,12);(6,13)]

//It is not tail recursive due to it not having an accumulator in its function parameters

let rec fT xs i acc = 
    match xs with
    | x::rest -> fT rest (i+1) (acc@[(x,i)])
    | [] -> acc

let rec fC xs i cont =
    match xs with
    | x::rest -> fC rest (i+1) (fun co -> cont((x,i) :: co))
    | [] -> cont []

// fT [3;4;5;6] 10 [];;
fC [3;4;5;6] 10 id;;

//problem 3

//Not all patterns that the match can catch is handled. If you are sure that your data wont trigger the incomplet branches then its fine. However it is better to ensure that all branches are handled
//

let p1 x = x>0;;
let rec f1 xys =
    match xys with
        | (x,y)::rest when p1 x -> x :: f1 rest
        | (x,y)::rest when not(p1 x) -> y :: f1 rest
        | [] -> [];;

//problem 4

type E = N of int | V of string | App of string * EList 
and EList = E list;;
let ex1 = App("f", [N 1; V "x"])
let ex2 = App("g", [ex1; V "y"; V "x"]) 

let rec vars e =
    match e with
    | N i -> []
    | V str -> [str]
    | App (str, list) -> varsList list
and varsList elist =
    match elist with
    | e::rest -> vars e @ varsList rest
    | [] -> []

let rec fnames e = 
    match e with
    | N i -> []
    | V str -> []
    | App (str, list) -> [str] @ fnamesList list
and fnamesList eList = 
    match eList with
    | e::rest -> fnames e @ fnamesList rest
    | [] -> []

let rec subst str i e =
    match e with
    | V v when v = str -> N i
    | App (n, list) -> App(n, substList str i list)
    | _ -> e
and substList  str i elist =
    match elist with
    | e::rest -> subst str i e :: substList str i rest
    | [] -> []

subst "x" 2 ex2

//problem 5
let rec f2 xs ys =
    match xs with
    | [] -> ys
    | x::rest -> f2 rest (x::ys);;


type T = A of int | B of T * T
let rec f3 t = 
    match t with
    | A i -> i
    | B(t1,t2) -> f3 t1 + f3 t2

//The type T is not enough to indicate which type it is. Needs to match with the actual types of the decleration