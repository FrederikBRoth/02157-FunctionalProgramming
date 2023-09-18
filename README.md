# Functional Programming

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
