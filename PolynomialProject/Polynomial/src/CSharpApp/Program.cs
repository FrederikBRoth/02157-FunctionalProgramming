//                                     Michael R. Hansen
//                                            14-08-2023

using System;

using Microsoft.FSharp.Collections; // where the type FSharpList<T> is found 

namespace ConsoleApp1
{
    class Program
    {
        static void Main(string[] args)
        {   int[] n1 = new int[] {1, 0, -3 };

            Polynomial.Poly p1 = Polynomial.ofArray(n1);
            Polynomial.Poly p2 = Polynomial.ofArray(new int[] { 0, 0, -2});
            Polynomial.Poly p3 = p1 + p2;

            Console.WriteLine("p1(x) is " + p1);
            Console.WriteLine("p2(x) is " + p2);
            Console.WriteLine("p1(x) + p2(x) is " + p3);

            Console.WriteLine("");
            Console.ReadKey();            
        }
    }
}
