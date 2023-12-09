// https://adventofcode.com/2023/day/8

type Node =
    { Id: string
      Left: string
      Right: string }

let notEmpty str = str <> ""

let endsWith (suffix: string) (str: string) = str.EndsWith suffix

let rec gcd a b = if b = 0L then a else gcd b (a % b)

let lcm a b =
    let res = (a * b) / gcd a b
    res

let walkWhile nodes (route: char array) stop origin =
    let rec walker steps id =
        if stop id then
            steps
        else
            let direction = route[steps % route.Length]
            let node = Map.find id nodes

            match direction with
            | 'L' -> walker (steps + 1) node.Left
            | 'R' -> walker (steps + 1) node.Right
            | _ -> failwith $"invalid direction {direction}"

    walker 0 origin

let part1 route nodes = walkWhile nodes route ((=) "ZZZ") "AAA"

let part2 route nodes =
    nodes
    |> Map.keys
    |> Seq.toList
    |> List.filter (endsWith "A")
    |> List.map (walkWhile nodes route (endsWith "Z"))
    |> List.map int64
    |> List.fold lcm 1L

let parseNode (str: string) =
    let splits = str.Split [| '='; ' '; ',' |] |> Array.filter notEmpty

    match splits with
    | [| node; left; right |] ->
        { Id = node
          Left = left[1..]
          Right = right[.. right.Length - 2] }
    | _ -> failwith $"invalid node format '{str}'"

let parseMap lines =
    let route = lines |> List.head |> Seq.toArray

    let nodes =
        lines
        |> List.tail
        |> List.filter notEmpty
        |> List.map parseNode
        |> List.fold (fun m node -> Map.add node.Id node m) Map.empty

    (route, nodes)

let main filename =
    let (route, nodes) =
        filename |> System.IO.File.ReadAllLines |> Array.toList |> parseMap

    let solution1 = part1 route nodes
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 route nodes
    printfn $"Solution 2: {solution2}"

//main "test.txt"
main "input.txt"
