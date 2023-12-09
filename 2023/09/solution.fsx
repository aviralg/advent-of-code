// https://adventofcode.com/2023/day/9

let all elt list =
    List.forall ((=) elt) list

let diff seq =
    seq |> List.pairwise |> List.map (fun (a, b) -> b - a)

let rec diffUntil f seq =
    if f seq then [ seq ] else seq :: diffUntil f (diff seq)

let getLast seqs =
    let folder sum seq = sum + List.last seq
    seqs |> List.fold folder 0

let part1 seqs =
    seqs |> List.map (diffUntil (all 0)) |> List.sumBy getLast

let getFirst seqs =
    let folder sum seq = List.head seq - sum
    seqs |> List.rev |> List.fold folder 0

let part2 seqs =
    seqs |> List.map (diffUntil (all 0)) |> List.sumBy getFirst

let parseSequence (str: string) =
    str.Split [| ' ' |] |> Array.map int |> Array.toList

let main filename =
    let sequences =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.map parseSequence
        |> Array.toList

    let solution1 = part1 sequences
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 sequences
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
