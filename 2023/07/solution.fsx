// https://adventofcode.com/2023/day/7

type Hand = { Cards: char list; Bid: int }

let parseHand (str: string) =
    match str.Split [| ' ' |] with
    | [| cards; bid |] ->
        { Cards = Seq.toList cards
          Bid = int bid }
    | _ -> failwith $"invalid hand: {str}"

let LetterMap1 = [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ]
let LetterMap2 = [ 'J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A' ]

let handValue map hand =
    let letterValue letter =
        List.findIndex (fun l -> l = letter) map

    List.map letterValue hand.Cards

let handKind { Cards = cards; Bid = bid } =
    let counts = cards |> List.countBy id |> List.sortBy snd

    match counts with
    | [ (_, 5) ] -> 7
    | [ (_, 1); (_, 4) ] -> 6
    | [ (_, 2); (_, 3) ] -> 5
    | [ (_, 1); (_, 1); (_, 3) ] -> 4
    | [ (_, 1); (_, 2); (_, 2) ] -> 3
    | [ (_, 1); (_, 1); (_, 1); (_, 2) ] -> 2
    | _ -> 1

let cardCompare handValue handKind hand1 hand2 =
    let kindDiff = handKind hand1 - handKind hand2

    if kindDiff <> 0 then
        kindDiff
    else
        List.zip (handValue hand1) (handValue hand2)
        |> List.map (fun (a, b) -> a - b)
        |> List.find (fun elt -> elt <> 0)

let listReplace e1 e2 l =
    List.map (fun e -> if e = e1 then e2 else e) l

let changeJ { Cards = cards; Bid = bid } =
    let largestCard =
        cards
        |> List.filter (fun e -> e <> 'J')
        |> List.countBy id
        |> List.append [ ('J', 0) ]
        |> List.sortBy snd
        |> List.last
        |> fst

    { Cards = listReplace 'J' largestCard cards
      Bid = bid }

let computeBid comparator hands =
    hands
    |> List.sortWith comparator
    |> List.indexed
    |> List.map (fun (factor, hand) -> (factor + 1) * hand.Bid)
    |> List.sum

let part1 hands =
    let comparator = cardCompare (handValue LetterMap1) handKind
    computeBid comparator hands

let part2 hands =
    let comparator = cardCompare (handValue LetterMap2) (changeJ >> handKind)
    computeBid comparator hands

let main filename =
    let hands =
        filename |> System.IO.File.ReadAllLines |> Array.toList |> List.map parseHand

    let solution1 = part1 hands
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 hands
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
