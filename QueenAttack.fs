module QueenAttack

let create (position: int * int) :bool =
    match position with
    | (x, _) when x < 0 || x > 7 -> false
    | (_, y) when y < 0 || y > 7 -> false
    | x -> true

let canAttack (queen1: int * int) (queen2: int * int) = 
    let (x, y) = queen1
    let (x2, y2) = queen2
    let difference = ( y2 - y, x2 - x)
    match difference with
    | (a, _) when a = 0 -> true
    | (_, b) when b = 0 -> true
    | (x, y) when y / x = 1 || y / x = -1 -> true
    | z -> false