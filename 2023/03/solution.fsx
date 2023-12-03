type Part =
    { Row: int
      Col1: int
      Col2: int
      Value: int }

let parseParts row (str: string) =
    let len = str.Length

    let rec advance col =
        if col < len && System.Char.IsDigit str[col] then
            advance (col + 1)
        else
            col

    let rec helper col parts =
        let col2 = advance col

        if col >= str.Length then
            List.rev parts
        elif col2 = col then
            helper (col + 1) parts
        else
            let part =
                { Row = row
                  Col1 = col
                  Col2 = col2 - 1
                  Value = int str[col .. (col2 - 1)] }

            helper col2 (part :: parts)

    helper 0 []

let adjacentLocations isValid part =
    let row, col1, col2 = part.Row, part.Col1, part.Col2
    let sides = [ (row, col1 - 1); (row, col2 + 1) ]
    let tops = [ for i in (col1 - 1) .. (col2 + 1) -> ((row - 1), i) ]
    let bottoms = [ for c in (col1 - 1) .. (col2 + 1) -> (row + 1, c) ]
    let l = sides @ tops @ bottoms
    (part, List.filter isValid l)

let isAdjacent (lines: string array) (num, locs) =
    let isSymbol ch =
        not <| ((ch = '.') || System.Char.IsDigit ch)

    let pred (row, col) = isSymbol <| lines[row][col]
    List.exists pred locs

let isSymbol (lines: string array) (r, c) =
    let ch = lines[r][c]
    not <| ((ch = '.') || System.Char.IsDigit ch)

let isValid rows cols (r, c) =
    r >= 0 && r < rows && c >= 0 && c < cols

let part1 (lines:string array) =
    let rows = Array.length lines
    let cols = lines[0].Length

    lines
    |> Array.mapi parseParts
    |> List.concat
    |> List.map (adjacentLocations (isValid rows cols))
    |> List.filter (snd >> List.exists (isSymbol lines))
    |> List.map (fst >> _.Value)
    |> List.sum

let findStars row (str:string) =
    [for col in 0 .. (str.Length - 1) do
        if str[col] = '*' then
            (row, col)]

let adjacentPoints (row, col) =
    [ (row - 1, col - 1)
      (row - 1, col)
      (row - 1, col + 1)
      (row, col - 1)
      (row, col + 1)
      (row + 1, col - 1)
      (row + 1, col)
      (row + 1, col + 1) ]

let intersects part (r, c) = r = part.Row && c >= part.Col1 && c <= part.Col2
let intersectsAny locs part = List.exists (intersects part) locs
let intersectingParts parts locs =
    List.filter (intersectsAny locs)  parts

let partProduct l =
    match l with
    | [p1; p2] -> p1.Value * p2.Value
    | _ -> 0

let part2 (lines:string array) =
    let parts = 
        lines
        |> Array.mapi parseParts
        |> List.concat

    lines 
    |> Array.mapi findStars
    |> List.concat
    |> List.map adjacentPoints
    |> List.map (intersectingParts parts)
    |> List.map partProduct 
    |> List.sum

let main filename =
    let lines = filename |> System.IO.File.ReadAllLines

    let solution1 = part1 lines
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 lines
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"