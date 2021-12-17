/// https://adventofcode.com/2021/day/17
module Day17
open System

type Range =
    {
        LowerBound: int
        UpperBound: int
    }
    member me.Contains (number: int) =
        me.LowerBound <= number && me.UpperBound >= number

let private readInput () =
    let splitInTwo = function [| x; y |] -> x, y | _ -> failwithf "Unexpected input"
    let input = System.IO.File.ReadAllText(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day17", "Input.txt"))
    let x, y = input.Replace("target area: ", "").Split(", ") |> splitInTwo
    let xRange = x.Replace("x=", "").Split("..") |> Array.map Int32.Parse |> Array.sort |> splitInTwo |> (fun (x1, x2) -> { LowerBound = x1; UpperBound = x2 })
    let yRange = y.Replace("y=", "").Split("..") |> Array.map Int32.Parse |> Array.sort |> splitInTwo |> (fun (y1, y2) -> { LowerBound = y1; UpperBound = y2 })
    xRange, yRange

let part1 () =
    let _, yTargetRange = readInput ()
    let maxY = -yTargetRange.LowerBound - 1

    printfn "Day 17 - Part 1 - Max height: %i" (maxY * (maxY + 1) / 2)

let part2 () =
    let xTargetRange, yTargetRange = readInput ()

    let minX = seq { 1..Int32.MaxValue } |> Seq.find (fun x -> xTargetRange.Contains(x * (x + 1) / 2))
    let maxX = xTargetRange.UpperBound
    let minY = yTargetRange.LowerBound
    let maxY = -yTargetRange.LowerBound - 1

    let hasMatchingStep (x, y) =
        let rec increaseStep (currentX, sumX, currentY, sumY) =
            if xTargetRange.Contains sumX && yTargetRange.Contains sumY then
                true
            elif sumX > xTargetRange.UpperBound || sumY < yTargetRange.LowerBound then
                false
            else
                let newXSpeed = Math.Max(0, currentX - 1)
                let newXSum = newXSpeed + sumX
                let newYSpeed = currentY - 1
                let newYSum = newYSpeed + sumY
                increaseStep (newXSpeed, newXSum, newYSpeed, newYSum)

        increaseStep (x, x, y, y)

    //Brute forcing: since we have minX, maxX, minY and maxY we only have a limited amount of options to check
    let nbPossibilities =
        [
            for x in minX..maxX do
                for y in minY..maxY do
                    if hasMatchingStep (x, y) then (x, y) else ()
        ]
        |> List.length

    printfn "Day 17 - Part 2 - Number of possibilities: %i" nbPossibilities