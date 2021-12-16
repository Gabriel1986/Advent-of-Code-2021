/// https://adventofcode.com/2021/day/13
module Day13
open System
open System.Collections.Generic

type FoldInstruction =
    | XAxis of int
    | YAxis of int

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (Int32.Parse (s.Substring(p.Length).Trim()))
    else
        None

let private readInput () =
    let inputStrings = System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day13", "Input.txt"))

    let dots =
        inputStrings
        |> Seq.takeWhile (fun x -> x <> "")
        |> Seq.map (fun coordinateString -> coordinateString.Split(",") |> (fun split -> Int32.Parse split[0], Int32.Parse split[1]))
        |> Set.ofSeq

    let foldInstructions =
        inputStrings
        |> Seq.rev
        |> Seq.takeWhile (fun x -> x <> "")
        |> Seq.map (
            function
            | Prefix "fold along x=" x -> FoldInstruction.XAxis x
            | Prefix "fold along y=" y -> FoldInstruction.YAxis y
            | other -> failwithf "Failed to parse fold instricution %s" other
        )
        |> Seq.rev

    {| Dots = dots; FoldInstructions = foldInstructions |}

let pivot (a: string array array): string array array =
    let maxXIndex = a.Length
    let maxYIndex = a[0].Length

    [|
        for i in 0..maxYIndex-1 do [|
            for j in 0..maxXIndex-1 do
                a[j][i]
        |]
    |]

let prettyPrint (a: string array array) =
    for i in 0..a.Length-1 do
        printfn "%s" (a[i] |> String.JoinWith "")

let fold (paper: string array array, foldInstruction: FoldInstruction) =
    match foldInstruction with
    | FoldInstruction.XAxis xFold ->
        [|
            let maxXIndex = paper.Length - 1
            for x in 0..xFold-1 do
                [|
                    let maxYIndex = paper[x].Length - 1
                    for y in 0..maxYIndex do
                        if paper[x][y] = "#" || paper[maxXIndex-x][y] = "#" then
                            "#"
                        else
                            "."
                |]
        |]
    | FoldInstruction.YAxis yFold ->
        [|
            for x in 0..paper.Length - 1 do
                [|
                    let maxYIndex = paper[x].Length - 1
                    for y in 0..yFold-1 do
                        if paper[x][y] = "#" || paper[x][maxYIndex-y] = "#" then
                            "#"
                        else
                            "."
                |]
        |]

let part1 () =
    let input = readInput ()
    let maxXIndex = input.FoldInstructions |> Seq.choose (function XAxis xAxisFold -> Some (xAxisFold * 2) | YAxis _ -> None) |> Seq.max
    let maxYIndex = input.FoldInstructions |> Seq.choose (function YAxis yAxisFold -> Some (yAxisFold * 2) | XAxis _ -> None) |> Seq.max

    let paper =
        [|
            for x in 0..maxXIndex do
                [|
                    for y in 0..maxYIndex do
                        if input.Dots.Contains((x, y)) then
                            "#"
                        else
                            "."
                |]
        |]

    let foldedPaper = fold (paper, Seq.head input.FoldInstructions)
    printfn "Day 13 - Part 1 - Number of dots: %i" (foldedPaper |> Array.sumBy (Array.sumBy (function "#" -> 1 | other -> 0)))

let part2 () =
    let input = readInput ()
    let maxXIndex = input.FoldInstructions |> Seq.choose (function XAxis xAxisFold -> Some (xAxisFold * 2) | YAxis _ -> None) |> Seq.max
    let maxYIndex = input.FoldInstructions |> Seq.choose (function YAxis yAxisFold -> Some (yAxisFold * 2) | XAxis _ -> None) |> Seq.max

    let paper =
        [|
            for x in 0..maxXIndex do
                [|
                    for y in 0..maxYIndex do
                        if input.Dots.Contains((x, y)) then
                            "#"
                        else
                            "."
                |]
        |]

    let foldedPaper = input.FoldInstructions |> Seq.fold (fun dots instruction -> fold (dots, instruction)) paper

    printfn "Day 13 - Part 2 - Result:"
    prettyPrint (pivot foldedPaper)