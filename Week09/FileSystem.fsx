type FileSys = Element list 
and Element = | File of (string * string) | Dir of string * FileSys;;

let d1 = Dir("d1",[File("a1","java");
    Dir("d2", [File("a2","fsx");
    Dir("d3", [File("a3","fs")])]);
    File("a4","fsx");
    Dir("d3", [File("a5","pdf")])]);;

let rec namesFileSys = function
    | [] -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement = function
    | File (n, f) -> [n + "." + f]
    | Dir(s,fs) -> s :: (namesFileSys fs);;

namesElement d1

let rec searchfilesys ext filesys = 
    match filesys with
    | [] -> set []
    | e::es -> Set.union (searchElement ext e)  (searchfilesys ext es)
and searchElement ext elem = 
    match elem with
        | File (n, f) -> if f = ext then set [n] else set []
        | Dir(s, fs) -> searchfilesys ext fs


let rec longNameFileSys filesys = 
    match filesys with
    | [] -> set []
    | e::rest -> Set.union (longNameElement e)  (longNameFileSys rest)
and longNameElement elem = 
    match elem with
    | File(n, f) -> Set.singleton (n + "." + f)
    | Dir(s, fs) -> 
        let var = longNameFileSys fs
        Set.map (fun path -> s + "\\" + path) var

searchElement "fsx" d1

longNameElement d1