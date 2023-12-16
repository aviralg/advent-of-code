// https://adventofcode.com/2023/day/14

type Orientation =
    | North
    | West
    | South
    | East

type Platform =
    { Content: string
      Rows: int
      Cols: int }

let dims orientation platform =
    match orientation with
    | North
    | South -> (platform.Rows, platform.Cols)
    | West
    | East -> (platform.Cols, platform.Rows)

let index orientation platform row col =
    match orientation with
    | North -> (platform.Rows - 1 - row) * platform.Cols + col
    | West -> (platform.Rows - 1 - col) * platform.Cols + (platform.Cols - 1 - row)
    | South -> row * platform.Cols + (platform.Cols - 1 - col)
    | East -> col * platform.Cols + row

let tilt orientation platform =
    let buffer = System.Text.StringBuilder(platform.Content)
    let get r c = buffer[index orientation platform r c]

    let set r c v =
        buffer[index orientation platform r c] <- v

    let swap r1 c1 r2 c2 =
        let a, b = get r1 c1, get r2 c2
        set r1 c1 b
        set r2 c2 a

    let (rows, cols) = dims orientation platform

    let rec helper pos row col =
        if row >= 0 then
            match get row col with
            | '.' -> helper (pos @ [ row ]) (row - 1) col
            | '#' -> helper [] (row - 1) col
            | 'O' ->
                match pos with
                | [] -> helper pos (row - 1) col
                | h :: t ->
                    swap row col h col
                    helper (t @ [ row ]) (row - 1) col
            | _ -> failwith "invalid character in platform"

    List.iter (helper [] (rows - 1)) [ 0 .. cols - 1 ]
    { platform with Content = buffer.ToString() }

let cycle platform =
    platform |> tilt North |> tilt West |> tilt South |> tilt East

let cycleFor limit platform =
    let rec helper iter history platform =
        if iter >= limit then
            platform
        else
            let p2 = cycle platform
            let content = p2.Content

            let iter2 =
                match List.tryFindIndex ((=) content) history with
                | None -> iter + 1
                | Some index ->
                    let cycle = index + 1
                    let q, r = System.Math.DivRem(limit - iter, cycle)
                    iter + q * cycle + 1

            helper iter2 (content :: history) p2

    helper 0 [ platform.Content ] platform

let load platform =
    let (rows, cols) = dims North platform

    let get (r, c) =
        platform.Content[index North platform r c]

    List.allPairs [ 0 .. rows - 1 ] [ 0 .. cols - 1 ]
    |> List.filter (fun loc -> get loc = 'O')
    |> List.map (fun loc -> fst loc + 1)
    |> List.sum

let part1 platform = platform |> tilt North |> load

let part2 platform = platform |> cycleFor 1000000000 |> load

let parsePlatform (str: string) =
    let lines = str.Trim().Split [| '\n' |]
    let content = String.concat "" lines

    { Content = content
      Rows = lines.Length
      Cols = lines[0].Length }

let main filename =
    let platform = filename |> System.IO.File.ReadAllText |> parsePlatform

    let solution1 = part1 platform
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 platform
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
