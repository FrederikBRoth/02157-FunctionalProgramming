# Exercise 4.16 - Determine function types

```
let rec f = function
    | (x, []) -> []
    | (x, y::ys) -> (x+y)::f(x-1, ys);;
let rec g = function
    | [] -> []
    | (x,y)::s -> (x,y)::(y,x)::g s;;
let rec h = function
    | [] -> []
    | x::xs -> x::(h xs)@[x];;
```

1. Function f is of type: int * list<int>
2. Function g is of type: list<(int*int)>
3. function h is of type list<int>

## The value of the following expressions

```
1. f(x, [y0,y1, . . . ,yn−1]), n ≥ 0
2. g[(x0, y0),(x1, y1), . . . ,(xn−1, yn−1)], n ≥ 0
3. h[x0,x1, . . . ,xn−1], n ≥ 0
```

```
1. f() = [(x+y0), (x-1+y1), . . . , (x-(n)+yn-1)] -> list<int>
2. g() = [(x0, y0),(y0,x0),(x1,y1),(y1,x1)...(xn,yn),(yn,xn)] -> list<int,int>
3. h() = [x0,x1, . . . ,xn−1, xn-2, xn-3, . . . ,x0] -> list<int>
``` 

# Exercise 4.17 - Determine function type
```
let rec p q = function
    | []-> []
    | x::xs -> 
        let ys = p q xs
        if q x then x::ys else ys@[x];;
```

1. The type of p is (int -> bool) * list

## The value of the following expression
```
p q [x0; x1; x3; . . . ; xn−1]

```
The value is list, but is controlled with a predicate. We don't know the predicates function so determing is hard.
However looking at the result

