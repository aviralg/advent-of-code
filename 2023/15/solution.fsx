// https://adventofcode.com/2023/day/15

type Operation =
    | Remove of int * string
    | ReplaceOrAdd of int * string * int

type Lens = string * int

let computeHash str =
    let helper cur chr = (17 * (cur + int chr)) % 256
    Seq.fold helper 0 str

let rec remove lenses label =
    match lenses with
    | (label2, _) :: rest when label2 = label -> rest
    | head :: rest -> head :: (remove rest label)
    | [] -> []

let rec replace lenses label focus =
    match lenses with
    | (label2, _) :: rest when label2 = label -> (label, focus) :: rest
    | head :: rest -> head :: (replace rest label focus)
    | [] -> [ (label, focus) ]

let applyOp (boxes: Lens list array) op =
    match op with
    | Remove(boxid, label) -> boxes[boxid] <- remove boxes[boxid] label
    | ReplaceOrAdd(boxid, label, focus) -> boxes[boxid] <- replace boxes[boxid] label focus

let parseOp (seq: string) =
    if seq.EndsWith "-" then
        let label = seq[0 .. seq.Length - 2]
        Remove(computeHash label, label)
    else
        match seq.Split [| '=' |] with
        | [| label; focus |] -> ReplaceOrAdd(computeHash label, label, int focus)
        | other -> failwith $"invalid operation {other}"

let focusingPower boxid lenses =
    let power slot (_, focus) = (1 + boxid) * (1 + slot) * focus
    lenses |> List.mapi power |> List.sum

let part1 seqs =
    seqs |> Array.map computeHash |> Array.sum

let part2 seqs =
    let boxes = Array.create 256 []

    Array.iter (parseOp >> applyOp boxes) seqs

    boxes |> Array.mapi focusingPower |> Array.sum

let parseSeqs (str: string) =
    str.Trim().Split [| ','; '\n' |] |> Array.map (fun s -> s.Trim())

let main filename =
    let seqs = filename |> System.IO.File.ReadAllText |> parseSeqs

    let solution1 = part1 seqs
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 seqs
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
