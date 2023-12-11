// https://adventofcode.com/2023/day/11

let dims array2d =
    (Array2D.length1 array2d - 1, Array2D.length2 array2d - 1)

let mapcoord rows cols factor (x, y) =
    let lineq count index = factor * count + index - count

    let map list index =
        let count = list |> List.filter ((>) index) |> List.length |> int64
        lineq count index

    (map rows (int64 x), map cols (int64 y))

let emptyCols space =
    let (rows, cols) = dims space

    let isEmpty col =
        List.forall (fun row -> space[row, col] = '.') [ 0..rows ]

    [ 0..cols ] |> List.filter isEmpty |> List.map int64

let emptyRows space =
    let (rows, cols) = dims space

    let isEmpty row =
        List.forall (fun col -> space[row, col] = '.') [ 0..cols ]

    [ 0..rows ] |> List.filter isEmpty |> List.map int64

let galaxies space =
    let (rows, cols) = dims space

    let hashCoord row =
        [ 0..cols ]
        |> List.filter (fun col -> space[row, col] = '#')
        |> List.map (fun col -> (row, col))

    List.collect hashCoord [ 0..rows ]

let rec tails list =
    match list with
    | [] -> []
    | elts -> elts :: tails (List.tail elts)

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let rec distances (input: (int64 * int64) list) =
    let helper list =
        match list with
        | origin :: rest -> List.map (distance origin) rest
        | [] -> []

    input |> tails |> List.collect helper

let distSum factor space =
    let rows = emptyRows space
    let cols = emptyCols space

    space
    |> galaxies
    |> List.map (mapcoord rows cols factor)
    |> distances
    |> List.sum

let part1 space = distSum 2L space

let part2 space = distSum 1000000L space

let main filename =
    let space =
        filename |> System.IO.File.ReadAllLines |> Array.map Seq.toArray |> array2D

    let solution1 = part1 space
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 space
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
