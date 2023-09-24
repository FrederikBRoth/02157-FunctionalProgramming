# Exercise 4.18
1. 
```
let rec f g = function
    | []-> []
    | x::xs -> g x :: f (fun y -> g(g y)) xs;;
```

The result is a (a -> b) -> arg -> list<a> 

The function f takes a function g which then in turn also takes an argument.

2. 
The value of the following statement
```
f g [x0; x1; x2; . . . ; xn−1]

[g1*1(x0); g1*2(x1); g1*4(x2); . . . ; gn(xn-1)]

```

g is called 

# Problem 2 [Exam Paper](https://www.imm.dtu.dk/~mire/FSharpBook/OldExams/02157-2015summer.pdf) 
1.
```
let rec mixMap f xs ys = 
    match (xs, ys) with
    | ([], []) -> []
    | (x::xs, y::yx) -> f (x, y) :: mixMap f xs ys 
```
2. 
```
let rec unmixMap f g xs = 
    match xs with
    | [] -> ([], [])
    | x::xs -> 
        let (left, right) = x
        let (f, g) = unmixMap f g xs
        (f left :: f, g right :: g)
```
3.
mixMap: 
`('a * 'b -> 'b) -> list<'a> -> list<'b> -> list<'c>` 
unmixMap: 
`('a -> 'b) -> ('c -> 'd) -> list<'a * 'c> -> list<'b> * list<'d>`
