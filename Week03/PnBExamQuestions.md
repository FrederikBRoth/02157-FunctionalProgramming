# Problem 1

1. 
```
let rec numberOf x ys = 
    match ys with
    | [] -> 0
    | y::ys -> if y = x then numberOf(x, ys) + 1 else numberOf(x)
```
2.
```
let rec positionOfH x index ys = 
    match ys with
    | [] -> 0
    | y::ys -> if x = y then index :: positionOfH(x, index+1, ys)
let positionOf x ys = positionOfH(x, 0, ys)
```
3.
```
let rec filterMap p f xs =
    | [] -> []
    | x::xs -> if p x then f x :: filterMap p f xs else filterMap p f xs 
```

## Post problem reflection

1. I declared the function as style 2 `f x ` but
called the function using style 1 `f(x)`.

Otherwise correct

2. Forgot `else` after `then`. Did the same mistake as above with style mixup
Otherwise correct

3. Forgot `match xs with` before patterns. Otherwise correct

# Problem 2
1.  
```

1.  ([], [1;2;3]) 
2.  splitAt 3 [1;2;3;4;5] =  
    ([1] :: [2], splitAt 2 [2;3;4;5])
    ([1;2] :: [3], splitAt 1 [3;4;5])
    ([1;2;3] :: [], splitAt 0 [4;5])
    ([1;2;3],[4;5])
3.  splitAt 4 [1;2;3]
    ([1] :: [2], splitAt 3 [2;3])
    ([1;2] :: [3], splitAt 2 [3])
    ([1;2;3] :: [], splitAt 1 [])
    ([1;2;3] :: [], splitAt 0 [])
    ([1;2;3], [])
```
2.
The type of splitAt is 'a list * 'a list
The recursion stopper returns a ([],[]) which follows the
above mentioned type.
3. 
Value: ([x0;x1;...;xk-1], [xk;xk+1;...;xn-1])

## Post problem reflection

1. Sucked at it, couldn't for the life of me get my head around it.
Finally got it after running it a few times. Need to be sure about
weird setups for recursion
2. Type detection is relativily straight forward
3. Also simple after getting the first one

# Problem 3

1.
```
    g [1;2;3;4;5] = 
~>  f([1;2;3;4;5], [])
~>  1 :: f([3;4;5], 2::[])
~>  1 :: 3 :: f([5], 4::[2])
~>  1 :: 3 :: 5 :: 4 :: 2
~>  [1;3;5;4;2]

```
2. 
f: list * list -> list
g: list -> list

g takes a list and takes every second number from start to end and then the remain from end to start

## Post problem reflection
1. Evaluation is rather simple, though i'd reckon this was an easy
recursion. Tried to do an evaluation with problem 2.1 but its hard
when the recursion is sat to a variable.

2. Typing is easy. What g computes i am unsure. Seems too simple of an
explaination but it might be alright.
