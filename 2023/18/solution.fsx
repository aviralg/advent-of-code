// https://adventofcode.com/2023/day/18

type Plan = {Direction: string; Amount: int64; Color: string}

let hexToInt64 (str:string) =
    System.Int64.Parse(str, System.Globalization.NumberStyles.HexNumber)

let executePlan (x, y) plan =
    match plan.Direction with
    | "R" -> (x + plan.Amount, y)
    | "L" -> (x - plan.Amount, y)
    | "D" -> (x, y + plan.Amount)
    | "U" -> (x, y - plan.Amount)
    | dir -> failwith $"invalid direction {dir}"

let rec execute origin plans =
    match plans with
    | plan::rest -> 
        let pt2 = executePlan origin plan
        pt2::execute pt2 rest
    | [] -> [] 

let computeArea polygon =
    let partial (x1,y1) (x2,y2) = int64 (y1 + y2) * int64 (x1 - x2)
    let polygon2 = (List.tail polygon) @ [List.head polygon]  

    List.map2 partial polygon polygon2 
    |> List.sum
    |> (fun x -> x / 2L)
    |> abs

let computePerimeter plans =
    plans
    |> List.map _.Amount
    |> List.sum

let part1 plans =
    let points = execute (0, 0) plans
    let area = computeArea points
    let perimeter = computePerimeter plans  
    area + 1L + perimeter/2L

let reinterpret {Direction = _; Amount = _; Color = color} =
    let dir =
        match color[color.Length-1] with
        | '0' -> "R"
        | '1' -> "D"
        | '2' -> "L"
        | '3' -> "U"
        | d -> failwith "invalid direction {d}"
    
    let amt = hexToInt64 color[0..color.Length-2];
    
    {Direction = dir; Amount = amt; Color = color}

let part2 plans = 
    plans
    |> List.map reinterpret
    |> part1

let parsePlan (str:string) =
    match str.Trim().Split [|' '|] with
    | [| dir; amt; col |] -> {Direction = dir; Amount = int64 amt; Color = col[2..col.Length-2]}
    | x -> failwith $"invalid plan {x}"

let main filename =
    let plans = filename |> System.IO.File.ReadAllLines |> Array.map parsePlan |> List.ofArray

    let solution1 = part1 plans 
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 plans
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
