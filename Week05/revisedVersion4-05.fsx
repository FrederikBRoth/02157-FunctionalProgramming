type ArticleCode = string
type ArticleName = string

type Price = int;;

type Register = (ArticleCode * (ArticleName*Price)) list;;

let reg = [("a1",("cheese",25));
    ("a2",("herring",4));
    ("a3",("soft drink",5)) ];;

type NoPieces = int;; // np where np >= 0
type Item = NoPieces * ArticleCode;;
type Purchase = Item list;;

let pur = [(3,"a2"); (1,"a1")];;

type Info = NoPieces * ArticleName * Price;;
type Infoseq = Info list;;
type Bill = Infoseq * Price;;

exception ArticleNotFound

let rec findArticle2 ac register = 
    match List.tryFind(fun (ac1,adesc) -> ac1 = ac) register with
    | Some (ac1, adesc) -> adesc
    | None -> raise ArticleNotFound


let makeBill2 reg pur =
    List.foldBack
        (fun (np, ac) (billTail, sumTail) ->
            let (aname, aprice) = findArticle2 ac reg
            let tprice = np * aprice
            ((np, aname, tprice) :: billTail, tprice + sumTail))
        pur
        ([], 0)

let rec findArticle ac = function
    | (ac1,adesc)::_ when ac=ac1 -> adesc
    | _::reg -> findArticle ac reg
    | _ -> failwith(ac + " is an unknown article code");;

let rec makeBill reg = function
    | [] -> ([],0)
    | (np,ac)::pur -> 
        let (aname,aprice) = findArticle ac reg
        let tprice = np*aprice
        let (billtl,sumtl) = makeBill reg pur
        ((np,aname,tprice)::billtl,tprice+sumtl);;

makeBill reg pur;;

makeBill2 reg pur;;