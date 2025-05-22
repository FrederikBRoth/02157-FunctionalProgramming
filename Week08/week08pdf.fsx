type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics";
"To mock a mockingbird";
"What is the name of this book"];;

let ls0 = [("Communication and concurrency", "Bob", 4);
    ("Programming in Haskell", "Paul", 2);
    ("Communicating Sequential processes", "Mary", 7);
    ("Elements of the theory of computation", "Dick", 1)];

let rec onShelf book shelf = 
    match shelf with
    | [] -> false
    | b :: rest -> if b = book then true else onShelf book rest

let rec toShelf book shelf = 
    match shelf with
    | [] -> []
    | b :: rest -> if b >= book then book :: b :: rest else b :: toShelf book rest

let rec fromShelf book shelf = 
    match shelf with
    | [] -> None
    | b :: rest -> if book = b then Some book else fromShelf book rest

let rec addLoan b n d ls = 
    let loan = (b, n, d)
    ls @ [loan]

let rec removeLoan b n ls = 
    match ls with
    | [] -> ls
    | (book, name, c) :: rest -> if book = b && name = n then removeLoan b n rest else (book, name, c) :: removeLoan b n rest

let rec reminders date ls = 
    match ls with
    | [] -> []
    | (b, n, d) :: rest -> if date > d then (b,n) :: reminders date rest else reminders date rest

let rec toLetters reminders = 
    match reminders with
    | [] -> []
    | (book, name) :: rest -> ("Dear " + name + "!" + "\n" +  "Please return \"" + book + "\".\n" + "Regards Robin") :: toLetters rest

let rec ToLettersMap reminders = 
    List.map(fun (book, name) -> "Dear " + name + "!" + "\n" +  "Please return \"" + book + "\".\n" + "Regards Robin") reminders

let remindersMap date ls = 
    List.foldBack(fun (b, n, d) acc -> if date > d then (b,n) :: acc else acc) ls []

let test1 = onShelf "To mock a mockingbird" sh0

let test2 = toShelf "J damn good book" sh0

let test3 = fromShelf "To mock a mockingbird" sh0

let test4 = addLoan "Nice new book" "Fred" 4 ls0

let test5 = removeLoan "Programming in Haskell" "Paul" ls0

let test6 = reminders 3 ls0

let test7 = toLetters test6
let test8 = ToLettersMap test6
let test9 = remindersMap 3 ls0


//problem 2

let rec f x = function
    | [] -> []
    | y::ys -> (x,y)::f x ys;;


//Based on higher order functions in list

let f2 x xs = 
    List.foldBack(fun elem acc -> (x, elem) :: acc) xs []


let test10 = f2 "a" [1;2;3] 

