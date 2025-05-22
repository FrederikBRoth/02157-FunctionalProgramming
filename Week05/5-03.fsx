let p1(x) = x > 0
let rec sum(p, xs) = List.fold(fun acc x -> if p x then acc + x else acc) 0 xs

let testvals = [1; 5; -2; 5; -10]

let result = sum( p1, testvals)