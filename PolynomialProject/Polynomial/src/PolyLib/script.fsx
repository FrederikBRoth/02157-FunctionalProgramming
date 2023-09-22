// Michael R. Hansen  14-08-2023
// An tiny script file


// An absolute path to PolyLib.dll
#I @"/home/fred/Documents/Uni/02157-FunctionalProgramming/PolynomialProject/Polynomial/"
#r @"/home/fred/Documents/Uni/02157-FunctionalProgramming/PolynomialProject/Polynomial/src/PolyLib/bin/Debug/net7.0/PolyLib.dll"

open Polynomial

let p1 = ofList [ 1; 2 ]

let p2 = ofList [ 3; 4; 5 ]

let p3 = p1 + p2
