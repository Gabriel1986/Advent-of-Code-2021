/// https://adventofcode.com/2021/day/6
module Day6
open System

let private readInput () =
    System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day6", "Input.txt"))
    |> Seq.toArray
    |> (fun x -> x[0].Split(",") |> Array.map Int32.Parse)

///Doesn't scale :(
let part1 () =
    let evolve (fish: int array, nbDays: int) =
        let rec evolve (fish: int array, currentDay: int) =
            if currentDay = nbDays then
                fish.Length
            else
                evolve (fish |> Array.collect (function 0 -> [| 6; 8 |] | other -> [| other - 1 |]), currentDay + 1)
        evolve (fish, 0)

    let nbDays = 80
    let nbFish = evolve (readInput (), nbDays)
    printfn "Day 6 - Part 1 - Number of fish after %i days: %i" nbDays nbFish

let part2 () =
    let input = readInput ()

    let initialGeneration: int64 array = [|
        for i = 0 to 8 do yield input |> Array.sumBy (fun x -> if x = i then 1 else 0)
    |]

    let evolve (generation: int64 array) =
        [|
            generation[1]
            generation[2]
            generation[3]
            generation[4]
            generation[5]
            generation[6]
            generation[7] + generation[0]
            generation[8]
            generation[0]
        |]

    let nbDays = 256
    let nbFish =
        seq { 1..nbDays }
        |> Seq.fold (fun generation _ -> evolve generation) initialGeneration
        |> Array.sum

    printfn "Day 6 - Part 2 - Number of fish after %i days: %i" nbDays nbFish