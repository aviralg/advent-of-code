// https://adventofcode.com/2023/day/9

let all elt list = Array.forall ((=) elt) list

let diff seq =
    seq |> Array.pairwise |> Array.map (fun (a, b) -> b - a)

let rec diffUntil f seq =
    if f seq then [ seq ] else seq :: diffUntil f (diff seq)

let getLast seqs =
    let folder sum seq = sum + Array.last seq
    seqs |> List.fold folder 0

let part1 seqs =
    seqs |> Array.map (diffUntil (all 0)) |> Array.sumBy getLast

let getFirst seqs =
    let folder seq sum = Array.head seq - sum
    List.foldBack folder seqs 0

let part2 seqs =
    seqs |> Array.map (diffUntil (all 0)) |> Array.sumBy getFirst

let parseSeq (str: string) = str.Split [| ' ' |] |> Array.map int

let main filename =
    let seqs = filename |> System.IO.File.ReadAllLines |> Array.map parseSeq

    let solution1 = part1 seqs
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 seqs
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
