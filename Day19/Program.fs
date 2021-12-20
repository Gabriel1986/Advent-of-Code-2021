/// https://adventofcode.com/2021/day/19
module Day19
open System

type Point = { X: int; Y: int; Z: int }
type Scanner = { Id: int; Location: Point; Beacons: Point list }

let private readInput (): Scanner list =
    let parseBeacon (str: string) =
        let split = str.Split(",")
        { X = Int32.Parse split[0]; Y = Int32.Parse split[1]; Z = Int32.Parse split[2] }

    System.IO.File.ReadAllLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day19", "Input.txt"))
    |> Array.skip 1
    |> Array.fold
        (fun (beaconLists: Point list list, currentBeaconList: Point list) (nextLine: string) ->
            if nextLine = "" then
                beaconLists, currentBeaconList
            elif nextLine.StartsWith "---" then
                (currentBeaconList::beaconLists), []
            else
                beaconLists, (parseBeacon nextLine)::currentBeaconList) ([], [])
    |> (fun (beaconLists, currentBeaconList) -> currentBeaconList::beaconLists)
    |> List.rev
    |> List.mapi (fun index beaconList -> { Id = index; Location = { X = 0; Y = 0; Z = 0 }; Beacons = beaconList |> List.rev })

let alignScanner (alignedScanners: Scanner list) (scannerToAlign: Scanner) =
    let multiply (scanner: Scanner) (timesX: int, timesY: int, timesZ: int) =
        {
            scanner with Beacons = scanner.Beacons |> List.map (fun point -> { point with X = point.X * timesX; Y = point.Y * timesY; Z = point.Z * timesZ })
        }

    let toVectors (alignedPoints: Point list) (toAlignPoints: Point list): Point list =
        let subtract (alignedPoint: Point) (toAlignPoint: Point) =
            {
                X = alignedPoint.X - toAlignPoint.X
                Y = alignedPoint.Y - toAlignPoint.Y
                Z = alignedPoint.Z - toAlignPoint.Z
            }

        let toVectors (alignedPoint: Point) =
            toAlignPoints
            |> List.map (fun each -> subtract alignedPoint each)

        List.collect toVectors alignedPoints

    let permute (scanner: Scanner): Scanner seq =
        seq {
            scanner
            { scanner with Beacons = scanner.Beacons |> List.map (fun b -> { X = b.X; Y = b.Z; Z = b.Y }) }
            { scanner with Beacons = scanner.Beacons |> List.map (fun b -> { X = b.X; Y = b.Y; Z = b.Z }) }
            { scanner with Beacons = scanner.Beacons |> List.map (fun b -> { X = b.Y; Y = b.X; Z = b.Z }) }
            { scanner with Beacons = scanner.Beacons |> List.map (fun b -> { X = b.Y; Y = b.Z; Z = b.X }) }
            { scanner with Beacons = scanner.Beacons |> List.map (fun b -> { X = b.Z; Y = b.X; Z = b.Y }) }
            { scanner with Beacons = scanner.Beacons |> List.map (fun b -> { X = b.Z; Y = b.Y; Z = b.X }) }
        }
        |> Seq.collect (fun each ->
            seq {
                (1, 1, 1)
                (1, 1, -1)
                (1, -1, 1)
                (1, -1, -1)
                (-1, 1, 1)
                (-1, 1, -1)
                (-1, -1, 1)
                (-1, -1, -1)
            }
            |> Seq.map (fun (x, y, z) ->
                { each with Beacons = each.Beacons |> List.map (fun each -> { X = each.X * x; Y = each.Y * y; Z = each.Z * z }) }
            )
        )

    let alignScanner (alignedScanner: Scanner): Scanner option =
        permute scannerToAlign
        |> Seq.tryPick (fun permutation ->
            let vectors = toVectors alignedScanner.Beacons permutation.Beacons
            let matches = vectors |> List.countBy id |> List.maxBy snd
            if snd matches >= 12 then
                let vector = fst matches
                let newBeacons =
                    permutation.Beacons
                    |> List.map (fun beacon -> { X = beacon.X + vector.X; Y = beacon.Y + vector.Y; Z = beacon.Z + vector.Z })
                Some { scannerToAlign with Location = vector ; Beacons = newBeacons }
            else
                None
        )

    List.tryPick alignScanner alignedScanners


let rec alignScanners (latestAlignedScanners: Scanner list, scannersToFind: Scanner list, alignedScanners: Scanner list) =
    let newlyAlignedScanners =
        scannersToFind
        |> List.choose (alignScanner latestAlignedScanners)

    let newScannersToFind = scannersToFind |> List.filter (fun scanner -> not (newlyAlignedScanners |> List.exists (fun found -> found.Id = scanner.Id)))
    match newScannersToFind with
    | [] -> alignedScanners @ newlyAlignedScanners
    | _ -> alignScanners (newlyAlignedScanners, newScannersToFind, alignedScanners @ newlyAlignedScanners)

let part1 () =
    let scanners = readInput()
    let alignedScanners = alignScanners ([ scanners[0] ], scanners[1..], [ scanners[0] ])

    let nbBeacons =
        alignedScanners
        |> List.collect (fun scanner -> scanner.Beacons)
        |> List.distinct
        |> List.length

    printfn "Day 19 - Part 1 - Nb unique beacons: %i" nbBeacons

let part2 () =
    let scanners = readInput ()
    let alignedScanners = alignScanners ([ scanners[0] ], scanners[1..], [ scanners[0] ])

    let max =
        alignedScanners
        |> List.fold (fun (listOfScannersToTry, currentMax) currentScanner ->
            let localMax =
                listOfScannersToTry
                |> List.map (fun scanner -> Math.Abs(currentScanner.Location.X - scanner.Location.X) + Math.Abs(currentScanner.Location.Y - scanner.Location.Y) + Math.Abs(currentScanner.Location.Z - scanner.Location.Z))
                |> List.max
            let newMax = if localMax > currentMax then localMax else currentMax
            listOfScannersToTry |> List.filter ((<>) currentScanner), newMax
        ) (alignedScanners, 0)
        |> snd

    printfn "Day 19 - Part 2 - Maximum Manhattan Distance: %i" max