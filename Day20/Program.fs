/// https://adventofcode.com/2021/day/20
module Day20
open System

let private readInput (): string * char[][] =
    let input = System.IO.File.ReadAllLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day20", "Input.txt"))
    let enhancer = input[0]
    let image = input[2..]
    enhancer,
        [|
            for i in 0..image.Length-1 do [|
                for j in 0..image[0].Length-1 do
                    image[i][j]
            |]
        |]

let enhance (enhancer: string) (image: char[][]) (iteration: int) =
    let originalMaxI = image.Length - 1
    let originalMaxJ = image[0].Length - 1

    [|
        for i in -1..originalMaxI+1 do [|
            for j in -1..originalMaxJ+1 do
                let square = String [|
                    for squareI in -1..1 do
                        for squareJ in -1..1 do
                            let originalI, originalJ = i + squareI, j + squareJ
                            if originalI < 0 || originalI > originalMaxI || originalJ < 0 || originalJ > originalMaxJ then
                                if enhancer[0] = '#' && iteration % 2 = 1 then '0' else '1'
                            elif image[originalI][originalJ] = '.' then
                                '0'
                            else
                                '1'
                |]

                enhancer[Convert.ToInt32(square, 2)]
        |]
    |]

let part1 () =
    let enhancer, image = readInput()

    let enhancedImage =
        image
        |> (fun expanded -> seq { 1..2 } |> Seq.fold (enhance (enhancer)) expanded)

    let nbLitPixels = enhancedImage |> Array.sumBy (Array.sumBy (function '#' -> 1 | _ -> 0))
    printfn "Day 20 - Part 1 - Nb lit pixels: %i" nbLitPixels

let part2 () =
    let enhancer, image = readInput()

    let enhancedImage =
        image
        |> (fun expanded -> seq { 1..50 } |> Seq.fold (enhance (enhancer)) expanded)

    let nbLitPixels = enhancedImage |> Array.sumBy (Array.sumBy (function '#' -> 1 | _ -> 0))
    printfn "Day 20 - Part 2 - Nb lit pixels: %i" nbLitPixels