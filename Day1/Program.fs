/// https://adventofcode.com/2021/day/1
module Day1
open System

let private readInput () =
    System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day1", "Input.txt"))
    |> List.ofSeq
    |> List.map (Int32.Parse)

let part1 () =
    let nbMeasurementsLargerThanPredecessor =
        readInput ()
        |> List.pairwise
        |> List.sumBy (fun (predecessor, current) -> if current > predecessor then 1 else 0)

    printfn "Day 1 - Part 1 - Number of measurements larger than predecessor: %i" nbMeasurementsLargerThanPredecessor

let part2 () =
    let input = readInput ()

    let slidingWindowSums =
        [
            for i in 0..(input.Length-3) do
                yield [ input[i]; input[i+1]; input[i+2] ] |> List.sum
        ]

    let nbMeasurementsLargerThanPredecessor =
        slidingWindowSums
        |> List.pairwise
        |> List.sumBy (fun (predecessor, current) -> if current > predecessor then 1 else 0)

    printfn "Day 1 - Part 2 - Number of measurements in windows of 3 larger than predecessor: %i" nbMeasurementsLargerThanPredecessor