// Draft of solutions for exam set, May 2024, in 02157
// Michael R. Hansen   05-12-2024


// Problem 1 

type Currency  = | DKK    // Danish krone  
                 | SEK    // Swedish krone

type Price     = float            // expected to be non-negative

type Item       = string
type PricedItem = Item * Price

type Register   = PricedItem list  // names are unique in a register, Prices in DKK
type Purchase   = Item list
type Bill       = PricedItem list * Price * Currency
type Rate       = float    // DKK to SEK 

let r = 1.56   // 1 DKK is 1,56 SEK, 1 SEK er 0,64 DKK)

let reg = ([("Ragusa", 22.0); ("Bounty", 57.0); ("Licorice",44.0)]);;

// 1.1
let rec priceOk reg = match reg with
                      | []                     -> true
                      | (_,p)::rest when p>0.0 -> priceOk rest
                      | _                      -> false;;
 
// 1.2 
let rec getPrice itm reg = 
  match reg with 
  | (itm1,p1)::_ when itm1=itm -> p1: Price
  | _::rest                    -> getPrice itm rest
  | []                         -> failwith ("Item not found" + itm)


let priceOf itm reg curr rt =
  let pr = getPrice itm reg
  match curr with
  | DKK -> pr
  | SEK -> pr*rt;;


// 1.3
let rec pricedItemsOf pur reg curr rt =
    match pur with 
    | []        -> []
    | itm::rest -> (itm,priceOf itm reg curr rt) :: pricedItemsOf rest reg curr rt;;  

// 1.4
let rec totalPrice pur reg curr rt = 
   match pur with 
   | []        -> 0.0
   | itm::rest -> priceOf itm reg curr rt + totalPrice rest reg curr rt;;
   
// 1.5 
let mkBill pur reg curr rt =
   let pitms = pricedItemsOf pur reg curr rt
   let tp    = totalPrice pur reg curr rt
   (pitms, tp, curr)


// 1.6 
let priceOk1 reg = List.forall (fun (_,p) -> p>0.0) reg

let pricedItemsOf1 pur reg curr rt =
   List.map (fun itm -> (itm, priceOf itm reg curr rt)) pur

let totalPrice1 pur reg curr rt =
   List.foldBack (fun itm acc -> priceOf itm reg curr rt + acc) pur 0.0;;
   

// 1.7  

// There is a unique rate for each currency, therefore Map is a natural model for rates. 
// To allow for an arbitrary number of correncies, they are chosen to be strings.

type CurrencyNew = string 
type RateNew     = Map<string, float>

let priceOfNew itm reg curr rt =
   let pr = getPrice itm reg 
   let r = Map.find curr rt
   pr*r;;


// Problem 2 

let rec f xs i = match xs with
                 | x::rest -> (x,i)::f rest (i+1)
                 | [] -> [];;

// 2.1 
// f: 'a list -> i: int -> ('a * int) list
(*
Due to the form of the declaration f xs i = ... 
we observe that the type of f has the form 
txs -> ti -> tv 
where txs is the your of xs, ti is the type of i 
and tv is the type of the match expression.  

From the first clause of the match exprssion we see that 
xs has a list type, say xs: 'a list, x: 'a. Furthermore, due 
to the expression i+1 we know that the type of i has type int. 
Thus, (x,i) has type 'a * int and the type tv of the value is ('a*int) list. 

There are no further type constraints from the clauses, so 
    'a list -> int -> ('a * int) list is the most general type.
*)

// 2.2 
// f [x_1; ...; x_n] i = [(x_1, i); ...; (x_n, i+n-1)]

// 2.3
(*
   f [3;4;5;6] 10
=> (3,10):: f [4;5;6] 11
=> (3,10)::(4,11):: f [5;6] 12
=> (3,10)::(4,11)::(5,12):: f [6] 13
=> (3,10)::(4,11)::(5,12)::(6,13):: f [] 14
=> (3,10)::(4,11)::(5,12)::(6,13)::[] = [(3,10);(4,11);(5,12);(6,13)]
*)

// 2.4 
// There is one recursive call in the body of the function. That call is 
// in the first clause of the match expression: ... -> (x,i) :: f rest (i+1)
// When that call terminates, the cons operation (x,i) :: _ must be evaluated. 
// Hence the recursive call is not the last operation evaluated in the body. 
// The call of f in (x,i) :: f rest (i+1) is not in tail position.

// Tail recursive version of f
let rec fA xs i acc = 
     match xs with
     | x::rest -> fA rest (i+1) ((x,i)::acc)
     | []      -> List.rev acc;;

// continuation passing style version of f
let rec fK xs i k = 
     match xs with
     | x::rest -> fK rest (i+1) (fun res -> k((x,i)::res))
     | []      -> k [];;

// Problem 3
let p1 x = x>0;;
let rec f1 xys =
   match xys with
   | (x,y)::rest when p1 x      -> x :: f1 rest
   | (x,y)::rest when not(p1 x) -> y :: f1 rest
   | [] -> [];;
(*
The compiler analyses the patterns of the match expression trying to decide
whether every list of integer pairs is covered by a pattern. 

It does, however, not perform an analysis of when clauses, and therefor the 
compiler does not detect that every non-empty list is covered by the two patterns. 
Hence the warning is issued. 

There is no need to revise the program. But one could remove "when not(p1 x)" 
without changing the meaning of f.
*)

// Problem 4

type E = N of int | V of string | App of string * EList
and  EList = E list;;

let ex1 = App("f", [N 1; V "x"])
let ex2 = App("g", [ex1; V "y"; V "x"])


// 4.1
let rec vars = 
   function 
   | N _ -> [] 
   | V x -> [x] 
   | App(_,el) -> varsEL el 
and varsEL = 
   function 
   | [] -> [] 
   | e::el -> vars e @ varsEL el
 
// 4.2
let rec fnames = function N _ -> [] | V x -> [] | App(f,el) -> f::fnamesEL el 
and fnamesEL = function | [] -> [] | e::el -> fnames e @ varsEL el

// 4.3

// let ex3 = subst "x" 2 ex2 = App("g",[App("f",[N 1; N 2]); V "y"; N 2])
let rec subst x n e = match e with 
                      | V y when x=y -> N n
                      | App(f,el)    -> App(f,substEL x n el)
                      | _            -> e
and substEL x n = function 
                  | []    -> [] 
                  | e::el -> subst x n e :: substEL x n el;;  

// 4.4
App ("g", [App ("f", [N 1; N 2]); V "y"; N 2])


// Problem 5

let rec f2 xs ys = 
   match xs with 
   | []      -> ys
   | x::rest -> f2 rest x::ys;;

// 5.1
(*
Notice that f2 rest x::ys means (f2 rest x) :: ys.  
The is ys is a list of type, say ys: 'a list and 
'a list is also the return type of the function, 
i.e. f2 rest x should have type 'a list.

However, the cons in (f2 rest x) :: ys requires that 
f2 rest x has type 'a - the type of the elements of ys. 

Therefore, the type error 'a and 'a list cannot be unified. 

Changing f2 rest x::ys to f2 rest (x::ys) will remove the type error. 
*)

  
type T = A of int | B of T * T;;

let rec f3 t = match t with 
               | T i      ->  i 
               | T(t1,t2) -> f3 t1 + f3 t2;;  

// 5.2
(*
The type T occurs twice in patterns at places where only constructors can occur.

The constructors from the type declaration are A and B. 

A corrected version of f3 is:

let rec f3 t = match t with 
               | A i      ->  i 
               | B(t1,t2) -> f3 t1 + f3 t2;;
*)


       



