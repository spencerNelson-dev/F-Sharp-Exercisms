module GradeSchool

type School = Map<int, string list>

let empty: School = 
    Map.empty


let add (student: string) (grade: int) (school: School): School =

    match school |> Map.tryFind grade with
    | Some value ->
        let newList = (student::value) |> List.sort   
        school.Add(grade, newList)
    | None -> school.Add(grade, [student])

let roster (school: School): string list =

    let rec roster' (acc: string list list) aList  =
        match aList with
        | [] -> acc
        | (x, y)::rest ->
            roster' (y::acc) rest 

    let mapList = Map.toList school

    roster' [] mapList
        |> List.rev
        |> List.concat

let grade (number: int) (school: School): string list = 
    
    match school |> Map.tryFind number with
    | Some students -> students
    | None -> []
