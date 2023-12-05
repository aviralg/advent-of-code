// https://adventofcode.com/2023/day/5

type Almanac =
    { SeedToSoil: list<int64 * int64 * int64>
      SoilToFertilizer: list<int64 * int64 * int64>
      FertilizerToWater: list<int64 * int64 * int64>
      WaterToLight: list<int64 * int64 * int64>
      LightToTemperature: list<int64 * int64 * int64>
      TemperatureToHumidity: list<int64 * int64 * int64>
      HumidityToLoation: list<int64 * int64 * int64> }

    static member Empty =
        { SeedToSoil = []
          SoilToFertilizer = []
          FertilizerToWater = []
          WaterToLight = []
          LightToTemperature = []
          TemperatureToHumidity = []
          HumidityToLoation = [] }

let pairList l =
    let rec helper l res =
        match l with
        | x :: y :: rest -> helper rest <| (x, y) :: res
        | [] -> List.rev res
        | [ _ ] -> failwith "cannot pair list with odd number of elements"

    helper l []

let trim (str: string) = str.Trim()

let rec search alist source =
    match alist with
    | [] -> source
    | (d, s, r) :: rest ->
        if source >= s && source < (s + r) then
            d + (source - s)
        else
            search rest source

let searcher almanac =
    search almanac.SeedToSoil
    >> search almanac.SoilToFertilizer
    >> search almanac.FertilizerToWater
    >> search almanac.WaterToLight
    >> search almanac.LightToTemperature
    >> search almanac.TemperatureToHumidity
    >> search almanac.HumidityToLoation

let notEmptyStr line = line <> ""

let makeRange (l: string array) : int64 * int64 * int64 =
    match l with
    | [| dest; source; range |] -> int64 dest, int64 source, int64 range
    | _ -> failwith $"invalid range input {l}"

let makeMap (lines: string list) =
    lines
    |> List.takeWhile (fun line -> System.Char.IsDigit(line[0]))
    |> List.map (fun line -> line.Split [| ' ' |])
    |> List.map makeRange

let parseSeeds (str: string) =
    str[ 6.. ].Split([| ' ' |])
    |> Array.filter notEmptyStr
    |> Array.map int64
    |> Array.toList

let parseSeedsAlmanac (lines: string list) =
    let rec helper lines seeds almanac =
        match lines with
        | "seed-to-soil map:" :: rest -> helper rest seeds { almanac with SeedToSoil = makeMap rest }
        | "soil-to-fertilizer map:" :: rest -> helper rest seeds { almanac with SoilToFertilizer = makeMap rest }
        | "fertilizer-to-water map:" :: rest -> helper rest seeds { almanac with FertilizerToWater = makeMap rest }
        | "water-to-light map:" :: rest -> helper rest seeds { almanac with WaterToLight = makeMap rest }
        | "light-to-temperature map:" :: rest -> helper rest seeds { almanac with LightToTemperature = makeMap rest }
        | "temperature-to-humidity map:" :: rest ->
            helper rest seeds { almanac with TemperatureToHumidity = makeMap rest }
        | "humidity-to-location map:" :: rest -> helper rest seeds { almanac with HumidityToLoation = makeMap rest }
        | head :: rest when head.StartsWith "seeds:" -> helper rest (parseSeeds head) almanac
        | head :: rest when System.Char.IsDigit(head[0]) -> helper rest seeds almanac
        | [] -> (seeds, almanac)
        | _ -> failwith $"invalid input: {lines}"

    helper lines [] Almanac.Empty

let part1 seeds almanac =
    seeds |> List.map (searcher almanac) |> List.min

let part2 seeds almanac =
    let searcher = searcher almanac

    let rec folder loc1 (initial, final) =
        if initial = final then
            loc1
        else
            let loc3 = min loc1 (searcher initial)
            folder loc3 (initial + 1L, final)

    seeds
    |> pairList
    |> List.map (fun (initial, size) -> (initial, initial + size - 1L))
    |> List.fold folder System.Int64.MaxValue

let main filename =
    let (seeds, almanac) =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.toList
        |> List.map trim
        |> List.filter notEmptyStr
        |> parseSeedsAlmanac

    let solution1 = part1 seeds almanac
    printfn $"Solution 1: {solution1}"

    let solution2 = part2 seeds almanac
    printfn $"Solution 2: {solution2}"

main "test.txt"
main "input.txt"
