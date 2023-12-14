// https://adventofcode.com/2023/day/13

type Pattern = char array array

type Reflection =
    | Horizontal of int
    | Vertical of int

let dims (pattern: Pattern) = (pattern.Length, pattern[0].Length)

let rotate (pattern: Pattern) =
    let rows, cols = dims pattern
    Array.init cols (fun col -> Array.init rows (fun row -> pattern[row][col]))

let matches (pattern: Pattern) rows =
    let count = min rows (pattern.Length - rows)

    let check index =
        pattern[rows - index] = pattern[rows + index - 1]

    List.forall check [ 1..count ]

let intersection (pattern: Pattern) =
    let rows, _ = dims pattern
    [ 1 .. rows - 1 ] |> List.filter (matches pattern)

let summarize pattern =
    let pattern2 = rotate pattern

    List.map Horizontal (intersection pattern)
    @ List.map Vertical (intersection pattern2)

let weigh reflection =
    match reflection with
    | Horizontal h -> h * 100
    | Vertical v -> v

let diff (before: Reflection list) (after: Reflection list) =
    match before, after with
    | [ x ], [] -> None
    | [ x ], [ y ] -> if x = y then None else Some y
    | [ x ], [ y; z ] -> if x = y then Some z else Some y
    | _, _ -> failwith $"invalid reflection values: {before}, {after}"

let flip (pattern: Pattern) row col =
    match pattern[row][col] with
    | '.' -> pattern[row][col] <- '#'
    | '#' -> pattern[row][col] <- '.'
    | _ -> failwith "invalid character in pattern"

let withFlip pattern row col f =
    flip pattern row col
    let res = f pattern
    flip pattern row col
    res

let clean (pattern: Pattern) =
    let before = summarize pattern
    let rows, cols = dims pattern

    let rec helper row col =
        if row = rows then
            failwith "couldn't clean"

        let after = withFlip pattern row col summarize

        match diff before after with
        | Some res -> res
        | None ->
            if col = cols - 1 then
                helper (row + 1) 0
            else
                helper row (col + 1)

    helper 0 0

let part1 (patterns: Pattern array) =
    patterns
    |> Array.map summarize
    |> Array.map List.head
    |> Array.map weigh
    |> Array.sum

let part2 (patterns: Pattern array) =
    patterns |> Array.map clean |> Array.map weigh |> Array.sum

let parsePattern (str: string) =
    str.Trim().Split [| '\n' |] |> Array.map Seq.toArray

let parsePatterns (str: string) =
    str.Trim().Split("\n\n") |> Array.map parsePattern

let main filename =
    let records = filename |> System.IO.File.ReadAllText |> parsePatterns

    let solution1 = part1 records
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 records
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
