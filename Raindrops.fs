module Raindrops

let rec convert' rules (stringSoFar:string) (intToProcess:int) =
    match rules with
    | [] -> stringSoFar
    | (number, sound)::rest when intToProcess % number = 0 -> convert' rest (stringSoFar + sound) intToProcess 
    | head::rest -> convert' rest stringSoFar intToProcess 

let convert (number: int): string = 
    let convertWithRules = convert' [(3, "Pling"); (5, "Plang");(7, "Plong")] ""

    let answer = convertWithRules number
    match answer with
    | "" -> string number
    | _ -> answer
