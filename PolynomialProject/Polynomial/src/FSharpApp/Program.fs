//  Michael R. Hansen     14-08-2023
open System

[<EntryPoint>]
let main argv =
   let p1 = Polynomial.ofList [1; 0; -3]
   let p2 = Polynomial.ofList [0; 0; -2]
   let p3 = p1 + p2 

   let _ = Console.WriteLine("p1(x) is " + string p1)
   let _ = Console.WriteLine("p2(x) is " + string p2)
   let _ = Console.WriteLine("p1(x) + p2(x) is " + string p3)
   
   let _  = Console.ReadKey()           
   0 // return an integer exit code