// https://adventofcode.com/2023/day/15

type Operation =
    | Remove of int * string
    | ReplaceOrAdd of int * string * int

type Lens = string * int

type Boxes = Map of int * Lens

let get boxes id =
    Map.tryFind id boxes |> Option.defaultValue []

let set boxes id value = Map.add id value boxes

let update boxes id f = get boxes id |> f |> set boxes id

let computeHash str =
    let helper cur chr = (17 * (cur + int chr)) % 256
    Seq.fold helper 0 str

let rec remove label lenses =
    match lenses with
    | (label2, _) :: rest when label2 = label -> rest
    | head :: rest -> head :: (remove label rest)
    | [] -> []

let rec replace label focus lenses =
    match lenses with
    | (label2, _) :: rest when label2 = label -> (label, focus) :: rest
    | head :: rest -> head :: (replace label focus rest)
    | [] -> [ (label, focus) ]

let applyOp boxes op =
    match op with
    | Remove(boxid, label) -> update boxes boxid (remove label)
    | ReplaceOrAdd(boxid, label, focus) -> update boxes boxid (replace label focus)

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
    seqs
    |> Array.map parseOp
    |> Array.fold applyOp Map.empty
    |> Map.map focusingPower
    |> Map.values
    |> Seq.sum

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