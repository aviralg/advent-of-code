
let notEmpty str = str <> ""

let quadSolve ([t; d]:list<double>) =
    let temp = sqrt (t * t - 4.0 * d) 
    let a = (t + temp)/2.0
    let b = (t - temp)/2.0
    (min a b, max a b)

let possibilities (a, b) = 
    let b2 = floor b 
    let a2 = ceil a
    let c = if b = b2 then -1.0 else 0
    let d = if a = a2 then -1.0 else 0
    b2 - a2 + 1.0 + c + d

let parseRaces (lines:string list) =
    lines
    |> List.map (fun s -> s.Split [|' '|])
    |> List.map (Array.toList >> List.filter notEmpty >> List.tail >> List.map double) 
    |> List.transpose

let part1 races =
    races
    |> List.map quadSolve
    |> List.map possibilities
    |> List.fold (fun a b -> a * b) 1.0

let part2 races =
    races
    |> List.transpose
    |> List.map (List.fold (fun a b -> a + string b) "")
    |> List.map double 
    |> quadSolve
    |> possibilities

let main filename =
    let races =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.toList
        |> parseRaces

    let solution1 = part1 races
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 races
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
