//                 Michael R. Hansen 14-08-2023

module Polynomial

[<Sealed>]
type Poly

val ofList: int list -> Poly
val ofArray: int[] -> Poly

type Poly with

    static member (+): Poly * Poly -> Poly
