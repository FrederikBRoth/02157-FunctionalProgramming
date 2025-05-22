type Appliance = string
type Usage = Appliance * int

type Price = int
type Tariff = Map<Appliance, Price>
let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

let priceList : (Appliance * Price) list = [(fst ad1, 10); (fst ad2, 20); (fst ad3, 5)]
let tariff : Tariff = Map.ofList priceList

let priceListAlt : (Appliance * Price) list = [(fst ad2, 20); (fst ad3, 5)]
let tariffAlt : Tariff = Map.ofList priceListAlt

let inv usagelist =
    not(List.exists(fun (appl, usage) -> usage < 0) usagelist)

let durationOf app usageList = 
    let (ap, u) = List.find(fun (appl, usage) -> appl = app) usageList
    u

let wellformed usageList = 
    let noteExceeded = not(List.exists(fun (appl, usage) -> usage > 24)usageList)
    noteExceeded && inv(usageList)

let delete a usageList = 
    List.filter(fun (app, usage) -> not(app = a)) usageList

let isDefined ats trf = 
    List.forall(fun (app, usage) -> Map.containsKey app trf) ats

exception NoTariffException
let priceOf ats trf = 
    List.foldBack(fun (app, usage) acc -> 
        match Map.tryFind app trf with
        | Some value -> acc + value
        | None -> raise NoTariffException
        ) ats 0

let test = inv ats

let test2 = durationOf"dishwasher" ats

let test3 = wellformed ats

let test4 = delete "washing machine" ats

let test5 = isDefined ats tariff
let test6 = isDefined ats tariffAlt

let test7 = priceOf ats tariff



