/// https://adventofcode.com/2021/day/14
module Day14
open System

let private readInput () =
    let inputStrings = System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day14", "Input.txt"))

    let polymer = Seq.head inputStrings

    let transformations =
        inputStrings
        |> Seq.skip 2
        |> Seq.map (fun each -> each.Split(" -> ") |> (fun x -> x[0], x[1][0]))
        |> Map.ofSeq

    {| Polymer = polymer; Transformations = transformations |}

module String =
    let windowed (windowSize: int) (str: string) = [|
        for i in 0..str.Length - windowSize do
            str[i..i + windowSize - 1]
    |]

let part1 () =
    let input = readInput ()

    let nbIterations = 10

    let resultPolymer =
        seq { 1..nbIterations }
        |> Seq.fold (fun polymer _ ->
            polymer
            |> String.windowed 2
            |> Seq.map (fun x -> string input.Transformations[x] + string x[1])
            |> String.JoinWith ""
            |> (fun x -> string polymer[0] + x)
        ) input.Polymer

    let characterCounts = resultPolymer |> Seq.countBy id

    let maxCharacterCount = characterCounts |> Seq.maxBy (fun (_, count) -> count) |> snd
    let minCharacterCount = characterCounts |> Seq.minBy (fun (_, count) -> count) |> snd

    printfn "Day 14 - Part 1 - Most common character count: %i, Least common character count: %i, subtracted: %i" maxCharacterCount minCharacterCount (maxCharacterCount - minCharacterCount)

let part2 () =
    let input = readInput ()

    let nbIterations = 40

    let polymerMap =
        input.Polymer
        |> String.windowed 2
        |> Seq.fold (fun polymerMap str -> polymerMap |> Map.change str (function Some x -> Some (x+1L) | None -> Some 1L)) Map.empty

    let countMap =
        input.Polymer
        |> Seq.fold (fun countMap character -> countMap |> Map.change character (function Some x -> Some (x+1L) | None -> Some 1L)) Map.empty

    let _, updatedCountMap =
        seq { 1..nbIterations }
        |> Seq.fold (fun (accPoly: Map<string, Int64>, accCount: Map<char, Int64>) _ ->
            accPoly
            |> Seq.fold (fun (accPolyInner, accCountInner) kvp ->
                let transformed1 = string kvp.Key[0] + string input.Transformations[kvp.Key]
                let transformed2 = string input.Transformations[kvp.Key] + string kvp.Key[1]
                let nbOccurences = kvp.Value

                accPolyInner
                |> Map.change transformed1 (function Some x -> Some (x+nbOccurences) | None -> Some nbOccurences)
                |> Map.change transformed2 (function Some x -> Some (x+nbOccurences) | None -> Some nbOccurences)
                ,
                accCountInner
                |> Map.change input.Transformations[kvp.Key] (function Some x -> Some (x+nbOccurences) | None -> Some nbOccurences)
            ) (Map.empty, accCount)
        ) (polymerMap, countMap)

    let maxCharacterCount = updatedCountMap |> Seq.maxBy (fun kvp -> kvp.Value) |> (fun x -> x.Value)
    let minCharacterCount = updatedCountMap |> Seq.minBy (fun kvp -> kvp.Value) |> (fun x -> x.Value)

    printfn "Day 14 - Part 2 - Most common character count: %i, Least common character count: %i, subtracted: %i" maxCharacterCount minCharacterCount (maxCharacterCount - minCharacterCount)