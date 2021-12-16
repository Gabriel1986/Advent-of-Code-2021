/// https://adventofcode.com/2021/day/9
module Day9
open System
open System.Collections.Generic

let part1 () =
    let heightMap: int[][] = [|
        for line in System.IO.File.ReadAllLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day9", "Input.txt")) do
            [|
                for character in line do
                    Int32.Parse (character.ToString())
            |]
    |]

    let lowPoints = seq {
        for i in 0..heightMap.Length-1 do
            for j in 0..heightMap[i].Length-1 do
                let currentNumber = heightMap[i][j]

                let allNeighborsAreLarger =
                    [
                        if i > 0 then heightMap[i-1][j] else Int32.MaxValue
                        if i < heightMap.Length-1 then heightMap[i+1][j] else Int32.MaxValue
                        if j > 0 then heightMap[i][j-1] else Int32.MaxValue
                        if j < heightMap[i].Length-1 then heightMap[i][j+1] else Int32.MaxValue
                    ]
                    |> List.forall (fun x -> currentNumber < x)

                if allNeighborsAreLarger then currentNumber else ()
    }

    printfn "Day 9 - Part 1 - Sum of the low points: %i" (lowPoints |> Seq.sumBy ((+) 1))

type Point =
    {
        Number: Int64
        Visited: bool
    }
    member me.Impassible = me.Number = 9

let part2 () =
    let heightMap = new List<List<Point>> [|
        for line in System.IO.File.ReadAllLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day9", "Input.txt")) do
            new List<Point> [|
                for character in line do
                    { Number = Int64.Parse (character.ToString()); Visited = false }
            |]
    |]

    let countBasin (i: int, j: int): Int64 =
        let rec visitNeighbor (i: int, j: int) =
            if i < 0 || i >= heightMap.Count || j < 0 || j >= heightMap[i].Count then
                0
            else
                let currentPoint = heightMap[i][j]
                if currentPoint.Impassible || currentPoint.Visited then
                    0
                else
                    heightMap[i][j] <- { currentPoint with Visited = true }
                    1 + visitNeighbor (i+1, j) + visitNeighbor (i, j+1) + visitNeighbor (i-1, j) + visitNeighbor (i, j-1)
        visitNeighbor (i, j)

    let basinSizes = seq {
        for i in 0..heightMap.Count-1 do
            for j in 0..heightMap[i].Count-1 do
                let currentNumber = heightMap[i][j]
                if currentNumber.Impassible || currentNumber.Visited then
                    ()
                else
                    countBasin (i, j)
    }

    printfn "Day 9 - Part 2 - Multiplication of the sizes of the basins: %i" (basinSizes |> Seq.sortDescending |> Seq.take 3 |> Seq.fold (fun acc next -> acc * next) (1L))