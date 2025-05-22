type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int

type Stack = int list

let intpInstr stack  inst =
    match inst with
    | ADD -> 
        let fst = List.item 0 stack
        let scn = List.item(1) stack
        let stack = List.removeAt 0 stack
        let stack = List.removeAt 0 stack
        let added = scn + fst
        List.insertAt 0 added stack
    | SUB ->
        let fst = List.item(0) stack
        let scn = List.item(1) stack
        let stack = List.removeAt 0 stack
        let stack =  List.removeAt 0 stack
        let added = scn - fst
        List.insertAt 0 added stack
    | SIGN ->
        let fst = List.item 0 stack
        let newStack = List.removeAt 0 stack
        let added = if fst <= 0 then fst else fst * -1 
        List.insertAt 0 added newStack
    | ABS ->
        let fst = List.item(0) stack
        let stack = List.removeAt 0 stack
        let abs = abs fst
        List.insertAt 0 abs stack
    | PUSH r ->
        List.insertAt 0 r stack 

let exec instlist = 
    let value = List.fold(fun acc inst -> intpInstr acc inst) [] instlist
    value.Head

let instructions = [PUSH 1; PUSH 2; ADD; PUSH 5; SUB; ABS; SIGN; SIGN;]
// Properties

exec instructions

type Expr = | C of int
            | X
            | Add of Expr * Expr
            | Sub of Expr * Expr
            | Minus of Expr
            | Abs of Expr

let rec sem exp i = 
    match exp with
    | C c -> c
    | X -> i
    | Add (l,r) -> sem l i + sem r i
    | Sub (l, r) -> sem l i - sem r i
    | Minus e -> - (sem e i)
    | Abs e -> abs (sem e i)

let example = Add (C 3, Sub (X, Abs (Minus (C 5))))

let test2 =  sem example 10

let rec compile e x =
    match e with
    | C n -> [PUSH n]
    | X -> [PUSH x]  // Substitute the value of X
    | Add (e1, e2) -> compile e1 x @ compile e2 x @ [ADD]
    | Sub (e1, e2) -> compile e1 x @ compile e2 x @ [SUB]
    | Minus e1 -> compile e1 x @ [SIGN]
    | Abs e1 -> compile e1 x @ [ABS]

let test3 = compile example 10

exec test3 = sem example 10