// https://adventofcode.com/2023/day/10

type FieldMap = char array array

let strReplace (pat: string) (rep: string) (str: string) =
    System.Text.RegularExpressions.Regex.Replace(str, pat, rep)

let inside (map: 'a array array) (x, y) =
    if x < 0 || x >= map.Length then false
    elif y < 0 || y >= map[0].Length then false
    else true

let update dist (x0, y0) (x1, y1) =
    if inside dist (x1, y1) then
        let curr = dist[x1][y1]
        let updated = dist[x0][y0] + 1

        if curr = -1 || updated < curr then
            dist[x1][y1] <- updated
            true
        else
            false
    else
        false

let lookup (map: FieldMap) (x, y) =
    if inside map (x, y) then map[x][y] else '.'

let rec origin (map: FieldMap) =
    let rec helper (x, y) =
        if map[x][y] = 'S' then (x, y)
        elif y = map[0].Length - 1 then helper (x + 1, 0)
        else helper (x, y + 1)

    helper (0, 0)

let up (x, y) = (x + 1, y)
let down (x, y) = (x - 1, y)
let right (x, y) = (x, y + 1)
let left (x, y) = (x, y - 1)

let rec nextCoords (map: FieldMap) coord =
    let tile = lookup map coord

    match tile with
    | '.' -> []
    | '|' -> [ up coord; down coord ]
    | '-' -> [ left coord; right coord ]
    | 'L' -> [ right coord; up coord ]
    | 'J' -> [ left coord; up coord ]
    | '7' -> [ left coord; down coord ]
    | 'F' -> [ right coord; down coord ]
    | 'S' ->
        let connected c =
            nextCoords map c |> List.exists ((=) coord)

        [ up coord; down coord; left coord; right coord ] |> List.filter connected
    | _ -> failwithf $"invalid tile '{tile}'"

let newmap row col init =
    Array.init row (fun i -> Array.init col (fun j -> init))

let computeDistance map =
    let (x0, y0) = origin map
    let row, col = map.Length, map[0].Length
    let dist = newmap row col -1
    dist[x0][y0] <- 0

    let rec helper coord =
        coord |> nextCoords map |> List.filter (update dist coord) |> List.iter helper

    helper (x0, y0)
    dist

let part1 map =
    map |> computeDistance |> Array.map Array.max |> Array.max

let enclosed (map: char array) (dist: int array) =
    let str =
        Array.map2 (fun t d -> if d = -1 then '.' else t) map dist
        |> System.String
        |> strReplace "F-*7" ""
        |> strReplace "L-*J" ""
        |> strReplace "F-*J" "|"
        |> strReplace "L-*7" "|"
        |> strReplace "S" "|"

    let folder (pipes, insiders) i =
        if str[i] = '.' && pipes % 2 = 1 then (pipes, insiders + 1)
        elif str[i] = '|' then (pipes + 1, insiders)
        else (pipes, insiders)

    [ 0 .. str.Length - 1 ] |> List.fold folder (0, 0) |> snd

let part2 map =
    map |> computeDistance |> Array.map2 enclosed map |> Array.sum

let main filename =
    let map =
        filename |> System.IO.File.ReadAllLines |> Array.map Seq.toArray |> Array.rev

    let solution1 = part1 map
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 map
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
