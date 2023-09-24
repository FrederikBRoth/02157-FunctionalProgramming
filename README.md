# Functional Programming
[Course Description](https://courses.compute.dtu.dk/02157/) 

## Concepts
### Evaluation

Going through the different steps that the function does in each iteration

Example from 2-2-pow.fsx:

```
    pow "test" 3
~>  "test" + (pow "test" 2)
~>  "test" + ("test" + (pow "test" 1))
~>  "test" + ("test" + ("test" + (pow "test" 0)))
~>  "test" + ("test" + ("test" + ("")))
~>  "test" + ("test" + "test")
~>  "test" + "testtest"
~>  "testtesttest"
```
## Solid ChatGPT answers

### Hello hope you're doing good! In F# you can either declare functions like `f(x,y)` or `f x y`. What is the difference?
Hello! I'm doing well, thank you. I hope you're doing well too.

In F#, you can indeed declare and call functions in two different ways:

1. **With Parentheses:** `f(x, y)`
2. **Without Parentheses:** `f x y`

The difference between these two syntax styles is primarily a matter of preference and readability. F# allows you to use both styles interchangeably, but they have slightly different implications:

1. **With Parentheses (`f(x, y)`):** This style is similar to how you would call a function in many other programming languages like C# or Python. It explicitly groups the arguments within parentheses. It can be useful when you want to make it clear that you are passing a tuple or a single argument that is itself an expression.

   ```fsharp
   let addTuple (x, y) = x + y
   let result = addTuple (3, 4) // Call with a tuple
   ```

2. **Without Parentheses (`f x y`):** This style is more in line with functional programming conventions and makes it look like you are applying a function to multiple arguments. It's generally considered more idiomatic in F#. It can be more concise and easier to read for functions with multiple arguments.

   ```fsharp
   let add x y = x + y
   let result = add 3 4 // Call without parentheses
   ```

In practice, many F# developers prefer the second style (without parentheses) because it aligns with functional programming principles and can make the code more readable, especially when dealing with curried functions or pipelining. However, the choice between the two styles ultimately depends on your personal coding style and the specific requirements of your project.

### When compiled the first function results in a typing of `a' * b' -> c'` while the second function results in `a' -> b' -> c'`. Why is that?

In F#, the difference in the function type signatures between the two styles of declaring functions is related to how function currying works.

1. **With Parentheses (`f(x, y)`):** This style creates a function that takes a single argument, which is a tuple. When you define `addTuple` as `let addTuple (x, y) = x + y`, you are actually defining a function that takes a single argument, a tuple `(x, y)`, and extracts the values `x` and `y` from that tuple to perform the addition. The type signature of this function is `(a' * b') -> c'`, indicating that it takes a tuple of two values and returns a result.

   ```fsharp
   let addTuple (x, y) = x + y
   ```

2. **Without Parentheses (`f x y`):** This style creates a curried function that takes multiple arguments one at a time. When you define `add` as `let add x y = x + y`, you are defining a function that takes one argument (`x`) and returns another function that takes one more argument (`y`) and performs the addition. The type signature of this curried function is `a' -> b' -> c'`, indicating that it takes one argument of type `a'`, returns a function that takes an argument of type `b'`, and finally returns a result of type `c'`.

   ```fsharp
   let add x y = x + y
   ```

Curried functions are a fundamental concept in functional programming, and they allow for partial application and function composition, among other benefits. The choice between these two styles often depends on how you want to use the functions in your code. Curried functions can be more flexible in certain scenarios, while tuple-style functions may be more appropriate in others, depending on your specific requirements.
