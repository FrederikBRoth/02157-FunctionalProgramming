// 1. 
// We have a tuple with three elements 
// The type of the different elements of the tuple is as follows.
// Number: string
// YearofBirth: int
// Description of interest: list<string>
// Description: string * int * list<string>
// Register: list<string * int * list<string>>
// Arangement: string * int * list<string> -> bool

// type Description = {Number: string; YearOfBirth: int; Themes: string list}
type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list

let catalogue : LuggageCatalogue = [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); 
                                                    ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]
exception NoRouteException of string

let rec findRoute(lid: Lid, cat : LuggageCatalogue) = 
    match cat with
    | [] -> raise (NoRouteException "Not route")
    | (l, r) :: rest -> if lid = l then r else findRoute(lid, rest)

let test1 = findRoute("DL 016-914", catalogue)

let rec inRoute(flight: Flight, route: Route) = 
    match route with
    | [] -> false
    | (f, a) :: rest -> if flight = f then true else inRoute(flight, rest)

let test2 = inRoute("DL 189", findRoute("DL 016-914", catalogue))


let rec withFlight(flight: Flight, lc: LuggageCatalogue) = 
    match lc with
    | [] -> []
    | (l, r) :: rest -> if inRoute(flight, r) then l :: withFlight(flight, rest) else withFlight(flight, rest)

let test3 = withFlight("DL 124", catalogue)

type ArrivalCatalogue = (Airport * Lid list) list

let ac : ArrivalCatalogue = [("ATL", ["DL 016-914"; "SK 222-142"]);
    ("BRU", ["DL 016-914"; "SK 222-142"]);
    ("JFK", ["SK 222-142"]);
    ("CPH", ["DL 016-914"])]

let rec addToAirport (airport: Airport) (lid: Lid) (ac: ArrivalCatalogue) : ArrivalCatalogue =
    match ac with
    | [] -> [(airport, [lid])]
    | (a, lids) :: rest ->
        if a = airport then
            (a, lid :: lids) :: rest
        else
            (a, lids) :: addToAirport airport lid rest


let rec extend (lid : Lid, route: Route, ac: ArrivalCatalogue) : ArrivalCatalogue =
    match route with
    | [] -> ac
    | (_, airport) :: restRoute ->
        let updatedAC = addToAirport airport lid ac
        extend (lid, restRoute, updatedAC)

let test4 = extend("DL 016-8585",[("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")], ac)

let rec toArrivalCatalogue(lc : LuggageCatalogue) : ArrivalCatalogue =
    match lc with
    | [] -> []
    | (lid, route) :: rest ->
        let restCatalogue = toArrivalCatalogue rest
        extend(lid, route, restCatalogue)

let test5 = toArrivalCatalogue(catalogue)