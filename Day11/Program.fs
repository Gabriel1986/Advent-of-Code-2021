/// https://adventofcode.com/2021/day/11
module Day11
open System
open System.Collections.Generic

let private readInput () = new List<List<int>> [|
    for line in System.IO.File.ReadAllLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day11", "Input.txt")) do
        new List<int> [|
            for character in line do Int32.Parse (character.ToString())
        |]
|]

let part1 () =
    let input = readInput ()

    let calculateNbFlashes (nbIterations: int) =
        let rec increaseNumber (i: int, j: int) =
            if (i < 0 || i = input.Count || j < 0 || j = input[i].Count) then
                ()
            else
                match input[i][j] with
                | 9 ->
                    input[i][j] <- 10
                    increaseNumber (i-1, j)
                    increaseNumber (i, j-1)
                    increaseNumber (i+1, j)
                    increaseNumber (i, j+1)
                    increaseNumber (i-1, j-1)
                    increaseNumber (i+1, j+1)
                    increaseNumber (i+1, j-1)
                    increaseNumber (i-1, j+1)
                | 10 ->
                    ()
                | other ->
                    input[i][j] <- other + 1
                    ()

        let countFlashesAndReset () =
            [
                for i in 0..input.Count - 1 do
                    for j in 0..input[i].Count - 1 do
                        if input[i][j] = 10 then
                            input[i][j] <- 0
                            1
                        else
                            0
            ]
            |> List.sum

        let rec calculateNbFlashes (nbFlashes: int, currentIteration: int) =
            if currentIteration = nbIterations then nbFlashes
            else
                for i in 0..input.Count-1 do
                    for j in 0..input.Count-1 do
                        increaseNumber (i, j)
                calculateNbFlashes (countFlashesAndReset (), currentIteration + 1)

        calculateNbFlashes (0, 0)

    let nbIterations = 100
    printfn "Day 11 - Part 1 - Iterations: %i Nb flashes: %i" nbIterations (calculateNbFlashes nbIterations)

let part2 () =
    let input = readInput ()
    let nbNumbersInInput = input.Count * input[0].Count

    let calculateSynchronizationPoint () =
        let rec increaseNumber (i: int, j: int) =
            if (i < 0 || i = input.Count || j < 0 || j = input[i].Count) then
                ()
            else
                match input[i][j] with
                | 10 ->
                    ()
                | 9 ->
                    input[i][j] <- 10
                    increaseNumber (i-1, j)
                    increaseNumber (i, j-1)
                    increaseNumber (i+1, j)
                    increaseNumber (i, j+1)
                    increaseNumber (i-1, j-1)
                    increaseNumber (i+1, j+1)
                    increaseNumber (i+1, j-1)
                    increaseNumber (i-1, j+1)
                | other ->
                    input[i][j] <- other + 1
                    ()

        let countFlashesAndReset () =
            [
                for i in 0..input.Count - 1 do
                    for j in 0..input[i].Count - 1 do
                        if input[i][j] = 10 then
                            input[i][j] <- 0
                            1
                        else
                            0
            ]
            |> List.sum

        let rec calculateSynchronizationPoint (nbFlashes: int, currentIteration: int) =
            if nbFlashes = nbNumbersInInput then currentIteration
            else
                for i in 0..input.Count-1 do
                    for j in 0..input.Count-1 do
                        increaseNumber (i, j)
                calculateSynchronizationPoint (countFlashesAndReset (), currentIteration + 1)

        calculateSynchronizationPoint (0, 0)

    printfn "Day 11 - Part 2 - Iteration where everything synchronizes: %i" (calculateSynchronizationPoint ())