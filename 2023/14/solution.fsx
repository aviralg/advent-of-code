// https://adventofcode.com/2023/day/14

type Platform = char array array

let dims (platform: Platform) = (platform.Length, platform[0].Length)

let dec1 x = x - 1
let inc1 x = x + 1

let toString (platform: Platform) =
    platform |> Array.map System.String.Concat |> Array.rev |> String.concat "\n"

let show (platform: Platform) = printfn "%s" (toString platform)
(*
    let (rows, cols) = dims platform
    for i in 0..rows-1 do
        for j in 0..cols-1 do
            printf "%c" (platform[rows-1-i][j])
        printf "\n"
*)
let cycle (platform: Platform) =
    let (rows, cols) = dims platform

    let swap (r1, c1) (r2, c2) =
        let temp = platform[r1][c1]
        platform[r1][c1] <- platform[r2][c2]
        platform[r2][c2] <- temp

    let rec vertical step pos row col =
        if row >= 0 && row < rows then
            match platform[row][col] with
            | '.' -> vertical step (pos @ [ row ]) (step row) col
            | '#' -> vertical step [] (step row) col
            | 'O' ->
                match pos with
                | [] -> vertical step pos (step row) col
                | h :: t ->
                    swap (row, col) (h, col)
                    vertical step (t @ [ row ]) (step row) col
            | _ -> failwith "invalid character in platform"

    let rec horizontal step pos row col =
        if col >= 0 && col < cols then
            match platform[row][col] with
            | '.' -> horizontal step (pos @ [ col ]) row (step col)
            | '#' -> horizontal step [] row (step col)
            | 'O' ->
                match pos with
                | [] -> horizontal step pos row (step col)
                | h :: t ->
                    swap (row, col) (row, h)
                    horizontal step (t @ [ col ]) row (step col)
            | _ -> failwith "invalid character in platform"

    List.iter (vertical dec1 [] (rows - 1)) [ 0 .. cols - 1 ]
    List.iter (fun row -> horizontal inc1 [] row 0) [ 0 .. rows - 1 ]
    List.iter (vertical inc1 [] 0) [ 0 .. cols - 1 ]
    List.iter (fun row -> horizontal dec1 [] row (cols - 1)) [ 0 .. rows - 1 ]


let copy (p: Platform) =
    let (rows, cols) = dims p

    let copyCol row =
        Array.map (fun col -> p[row][col]) [| 0 .. cols - 1 |]

    Array.map copyCol [| 0 .. rows - 1 |]

let tilt (p: Platform) =
    let (rows, cols) = dims p
    let platform = copy p

    let swap (r1, c1) (r2, c2) =
        let temp = platform[r1][c1]
        platform[r1][c1] <- platform[r2][c2]
        platform[r2][c2] <- temp

    let rec helper pos row col =
        if row >= 0 then
            match platform[row][col] with
            | '.' -> helper (pos @ [ row ]) (row - 1) col
            | '#' -> helper [] (row - 1) col
            | 'O' ->
                match pos with
                | [] -> helper pos (row - 1) col
                | h :: t ->
                    swap (row, col) (h, col)
                    helper (t @ [ row ]) (row - 1) col
            | _ -> failwith "invalid character in platform"

    List.iter (helper [] (rows - 1)) [ 0 .. cols - 1 ]
    platform

let load platform =
    let (rows, cols) = dims platform
    let total n = List.sum [ (cols - n + 1) .. cols ]

    let count chr col =
        [ 0 .. rows - 1 ]
        |> List.map (fun row -> if platform[row][col] = chr then row + 1 else 0)
        |> List.sum

    [ 0 .. cols - 1 ] |> List.map (count 'O') |> List.sum

let part1 p =
    let platform = tilt p
    load p

let part2 platform =
    let v = ResizeArray([ toString platform ])
    let mutable iter = 1
    let limit = 1000000000

    while iter <= limit do
        cycle platform
        //printfn "load: %A" (load platform)
        let str = toString platform

        match Seq.tryFindIndex (fun s -> s = str) v with
        | None -> ()
        | Some index ->
            let cycle = v.Count - index
            let q, r = System.Math.DivRem(limit - iter, cycle)
            iter <- iter + q * cycle
        //printfn $"{index}, {cycle}, {q}, {r}"
        v.Add(str)
        iter <- iter + 1

    load platform

let parsePlatform (str: string) =
    str.Trim().Split [| '\n' |] |> Array.rev |> Array.map Seq.toArray

let main filename =
    let platform = filename |> System.IO.File.ReadAllText |> parsePlatform

    let solution1 = part1 platform
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 platform
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
