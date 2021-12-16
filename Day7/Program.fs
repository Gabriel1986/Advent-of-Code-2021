/// https://adventofcode.com/2021/day/7
module Day7
open System

let private readInput () =
    System.IO.File.ReadAllText(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day7", "Input.txt")).Split(",") |> Array.map Int32.Parse

let part1 () =
    let input = readInput ()

    let sorted = Array.sort input
    let median = sorted[Math.Ceiling(float sorted.Length / 2.) |> int]

    let sumOfDifferences = input |> Array.fold (fun fuelUsed next -> fuelUsed + Math.Abs(next - median)) 0

    printfn "Day 7 - Part 1 - Least amount of fuel used: %i" sumOfDifferences

let part2 () =
    let input = readInput ()

    let calculateMinDistance (input: int[], minValue: int) =
        let rec calculateMinDistance (input: int[], minDistance: int, currentValue: int) =
            let sumOfDifferences =
                input
                |> Array.sumBy (fun x ->
                    let n = Math.Abs (x - currentValue)
                    n * (n + 1) / 2)
            match minDistance < sumOfDifferences with
            | true -> minDistance
            | false -> calculateMinDistance (input, sumOfDifferences, currentValue + 1)

        calculateMinDistance (input, Int32.MaxValue, minValue)

    let minDistance = calculateMinDistance (input, Array.min input)
    printfn "Day 7 - Part 2 - Least amount of fuel used: %i" minDistance