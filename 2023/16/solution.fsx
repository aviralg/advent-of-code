// https://adventofcode.com/2023/day/16

type Contraption = { Space: string; Rows: int; Cols: int }

type Direction =
    | Up
    | Down
    | Left
    | Right

let get contraption (row, col) =
    contraption.Space[row * contraption.Cols + col]

let up (row, col) = (row - 1, col)

let down (row, col) = (row + 1, col)

let left (row, col) = (row, col - 1)

let right (row, col) = (row, col + 1)

let valid contraption (r, c) =
    r >= 0 && r < contraption.Rows && c >= 0 && c < contraption.Cols

let step contraption (dir, pos) =
    let c = get contraption pos

    match c, dir with
    | '.', Up -> [ (Up, up pos) ]
    | '.', Down -> [ (Down, down pos) ]
    | '.', Right -> [ (Right, right pos) ]
    | '.', Left -> [ (Left, left pos) ]

    | '/', Up -> [ (Right, right pos) ]
    | '/', Down -> [ (Left, left pos) ]
    | '/', Right -> [ (Up, up pos) ]
    | '/', Left -> [ (Down, down pos) ]

    | '\\', Up -> [ (Left, left pos) ]
    | '\\', Down -> [ (Right, right pos) ]
    | '\\', Right -> [ (Down, down pos) ]
    | '\\', Left -> [ (Up, up pos) ]

    | '|', Up -> [ (Up, up pos) ]
    | '|', Down -> [ (Down, down pos) ]
    | '|', Right
    | '|', Left -> [ (Up, up pos); (Down, down pos) ]

    | '-', Up
    | '-', Down -> [ (Right, right pos); (Left, left pos) ]
    | '-', Right -> [ (Right, right pos) ]
    | '-', Left -> [ (Left, left pos) ]

    | _ -> failwith $"invalid configuration ({c}, {dir})"

let beam contraption dir pos =
    let rec helper hist (dir, pos) =
        if List.exists ((=) (dir, pos)) hist then
            hist
        else
            step contraption (dir, pos)
            |> List.filter (snd >> valid contraption)
            |> List.fold helper ((dir, pos) :: hist)

    helper [] (dir, pos)

let energized contraption (dir, pos) =
    beam contraption dir pos
    |> List.map snd
    |> List.fold (fun set v -> Set.add v set) Set.empty
    |> Set.count

let part1 contraption = energized contraption (Right, (0, 0))

let triplets dir xs ys =
    List.allPairs [ dir ] (List.allPairs xs ys)

let part2 contraption =
    let rows, cols = contraption.Rows - 1, contraption.Cols - 1
    let l1 = triplets Down [ 0 ] [ 0..cols ]
    let l2 = triplets Up [ rows ] [ 0..cols ]
    let l3 = triplets Right [ 0..rows ] [ 0 ]
    let l4 = triplets Left [ 0..rows ] [ cols ]

    l1 @ l2 @ l3 @ l4 |> List.map (energized contraption) |> List.max

let parseContraption (str: string) =
    let lines = str.Trim().Split [| '\n' |]
    let space = String.concat "" lines

    { Space = space
      Rows = lines.Length
      Cols = lines[0].Length }

let main filename =
    let seqs = filename |> System.IO.File.ReadAllText |> parseContraption

    let solution1 = part1 seqs
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 seqs
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
