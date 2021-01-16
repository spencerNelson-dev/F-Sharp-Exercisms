module Accumulate

// easy answer: List.map func input

// create my own reverse list function
let rec myRev acc input =
    match input with
    | [] -> acc
    | head::rest -> myRev (head::acc) rest 

let revList x = myRev [] x

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    
    let rec accum funct input initial =
        
        match input with
        | [] -> revList initial
        | head::rest -> accum func rest ((funct head)::initial)

    accum func input []
