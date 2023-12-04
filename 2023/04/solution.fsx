// https://adventofcode.com/2023/day/4

type Card = {Id: int; Wins : int list ; Haves: int list;}

let parseCard (str:string) =
    let parseNums (str:string) = 
        str.Split [|' '|] |> 
        List.ofArray |> 
        List.filter (fun str -> str <> "") |> 
        List.map int
    let fields = str.Split [|':'|]
    let id = int <| fields[0][5..]
    let nums = fields[1].Split [|'|'|]
    let wins = parseNums nums[0]
    let haves = parseNums nums[1]
    {Id = id; Wins = wins; Haves = haves;}

let intersectionCount card =
    Set.intersect (Set.ofList card.Wins) (Set.ofList card.Haves) |>
    Set.count

let part1 cards =
    cards
    |> List.map intersectionCount 
    |> List.map (fun n -> pown 2 (n - 1))
    |> List.sum

let part2 cards =
    let makeSeq id n = (id, [(id + 1) .. (id + n)])
    let folder (acc:int array) (id, cards) =
        List.iter (fun i -> acc[i-1] <- acc[id-1] + acc[i-1]) cards
        acc

    cards
    |> List.map (fun card -> makeSeq card.Id (intersectionCount card))
    |> List.fold folder (Array.init (List.length cards) (fun n -> 1))
    |> Array.sum

let main filename =
    let cards =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.map parseCard
        |> Array.toList

    let solution1 = part1 cards
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 cards
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"