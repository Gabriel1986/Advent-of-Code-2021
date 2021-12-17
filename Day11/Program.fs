/// https://adventofcode.com/2021/day/11
module Day11
open System

let private readInput () =
    let lines = System.IO.File.ReadAllLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day11", "Input.txt"))
    Array2D.init (lines.Length) (lines[0].Length) (fun i j -> Int32.Parse (string (lines[i][j])))

let part1 () =
    let input = readInput ()

    let calculateNbFlashes (nbIterations: int) =
        let rec increaseNumber (i: int, j: int) =
            if (i < 0 || i = input.GetLength(0) || j < 0 || j = input.GetLength(1)) then
                ()
            else
                match input[i, j] with
                | 9 ->
                    input[i, j] <- 10
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
                    input[i, j] <- other + 1
                    ()

        let countFlashesAndReset () =
            [
                for i in 0..input.GetUpperBound(0) do
                    for j in 0..input.GetUpperBound(1) do
                        if input[i, j] = 10 then
                            input.SetValue (0, i, j)
                            1
                        else
                            0
            ]
            |> List.sum

        let rec calculateNbFlashes (nbFlashes: int, currentIteration: int) =
            if currentIteration = nbIterations then nbFlashes
            else
                for i in 0..input.GetUpperBound(0) do
                    for j in 0..input.GetUpperBound(1) do
                        increaseNumber (i, j)
                calculateNbFlashes (countFlashesAndReset (), currentIteration + 1)

        calculateNbFlashes (0, 0)

    let nbIterations = 100
    printfn "Day 11 - Part 1 - Iterations: %i Nb flashes: %i" nbIterations (calculateNbFlashes nbIterations)

let part2 () =
    let input = readInput ()
    let nbNumbersInInput = input.Length

    let calculateSynchronizationPoint () =
        let rec increaseNumber (i: int, j: int) =
            if (i < 0 || i = input.GetLength(0) || j < 0 || j = input.GetLength(1)) then
                ()
            else
                match input[i, j] with
                | 10 ->
                    ()
                | 9 ->
                    input.SetValue(10, i, j)
                    increaseNumber (i-1, j)
                    increaseNumber (i, j-1)
                    increaseNumber (i+1, j)
                    increaseNumber (i, j+1)
                    increaseNumber (i-1, j-1)
                    increaseNumber (i+1, j+1)
                    increaseNumber (i+1, j-1)
                    increaseNumber (i-1, j+1)
                | other ->
                    input.SetValue(other + 1, i, j)
                    ()

        //Using mutable here to not have to go over the entire array twice...
        let countFlashesAndReset () =
            let mutable counter = 0
            input
            |> Array2D.iteri (fun i j value ->
                if value = 10 then
                    counter <- counter + 1
                    input.SetValue(0, i, j)
                else
                    ()
            )
            counter

        let rec calculateSynchronizationPoint (nbFlashes: int, currentIteration: int) =
            if nbFlashes = nbNumbersInInput then currentIteration
            else
                input |> Array2D.iteri (fun i j _ -> increaseNumber (i, j))
                calculateSynchronizationPoint (countFlashesAndReset (), currentIteration + 1)

        calculateSynchronizationPoint (0, 0)

    printfn "Day 11 - Part 2 - Iteration where everything synchronizes: %i" (calculateSynchronizationPoint ())