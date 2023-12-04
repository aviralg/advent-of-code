// https://adventofcode.com/2023/day/1

let digitNames =
    [ ("zero", 0)
      ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9) ]

let inc x = x + 1

let dec x = x - 1

let firstIndex (str: string) (sub: string) =
    match str.IndexOf(sub) with
    | -1 -> str.Length
    | i -> i

let lastIndex (str: string) (sub: string) = str.LastIndexOf(sub)

let rec literalDigit (str: string) absent index next =
    if index < 0 || index = str.Length then
        (absent, 0)
    elif System.Char.IsDigit(str[index]) then
        (index, int (string str[index]))
    else
        literalDigit str absent (next index) next

let anyDigit (str: string) literal finder comp =
    let folder (index1, value1) (digitName: string, value2) =
        let index2 = finder str digitName

        if comp index1 index2 then
            (index1, value1)
        else
            (index2, value2)

    List.fold folder (literal str) digitNames

let literalLeft str = literalDigit str str.Length 0 inc

let namedLeft str = anyDigit str literalLeft firstIndex (<)

let literalRight str = literalDigit str 0 (str.Length - 1) dec

let namedRight str = anyDigit str literalRight lastIndex (>)

let makeNumber ((i1, v1), (i2, v2)) = v1 * 10 + v2

let part1 input =
    input
    |> List.map (fun str -> (literalLeft str, literalRight str))
    |> List.map makeNumber
    |> List.sum

let part2 input =
    input
    |> List.map (fun str -> (namedLeft str, namedRight str))
    |> List.map makeNumber
    |> List.sum

let main filename =
    let input = filename |> System.IO.File.ReadLines |> List.ofSeq

    let solution1 = part1 input
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 input
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
