// https://adventofcode.com/2023/day/2

type Ball =
    | Red of int
    | Blue of int
    | Green of int
    with
    static member FromString (str:string) =
        match str.Split [|' '|] with
        | [|digits; color|] -> 
            let count = int digits
            match color with
            | "red" -> Red count
            | "green" -> Green count
            | "blue" -> Blue count
            | _ -> failwith $"Invalid color: {color}"
        | _ -> failwith $"Invalid ball: {str}"
 
type Game =
    {Id: int; Balls: Ball array }
    with
    static member FromString (str:string) =
        let left = 5
        let mutable right = left 
        while System.Char.IsDigit(str[right]) do
            right <- right + 1
        let id = int str[left..(right-1)]

        let balls = 
            str[(right + 1) ..].Split [|','; ';'|] |>
            Array.map _.Trim() |>
            Array.map Ball.FromString

        {Id = id; Balls = balls} 

let gameSatisfies r0 g0 b0 game =
    let pred ball =
        match ball with
        | Red r -> r > r0
        | Green g -> g > g0
        | Blue b -> b > b0
    Array.exists pred game.Balls |> not

let part1 games =
    games |>
    Seq.filter (gameSatisfies 12 13 14) |>
    Seq.map _.Id |>
    Seq.sum

let maxBalls game =
    let folder (r0, g0, b0) ball =
        match ball with
        | Red r -> (max r0 r, g0, b0)
        | Green g -> (r0, max g0 g, b0)
        | Blue b -> (r0, g0, max b0 b)
    Seq.fold folder (0, 0, 0) game.Balls

let part2 games =
    games |>
    Seq.map maxBalls |>
    Seq.map (fun (r, g, b) -> r * g * b) |>
    Seq.sum

let main filename =
    let games = 
        filename |>
        System.IO.File.ReadAllLines |> 
        Array.map Game.FromString

    let solution1 = part1 games
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 games
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"