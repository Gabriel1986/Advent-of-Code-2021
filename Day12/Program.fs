/// https://adventofcode.com/2021/day/12
module Day12
open System

type Cave =
    | Start
    | End
    | LargeCavern of string
    | SmallCave of string

let private readInput () =
    let parseCavern (str: string) =
        match str with
        | "start" -> Start
        | "end" -> End
        | x when x |> String.forall Char.IsLower -> SmallCave x
        | x when x |> String.forall Char.IsUpper -> LargeCavern x
        | other -> failwithf "Unexpected string: %s" other

    let createLinks (leftCavernString: string, rightCavernString: string) =
        seq {
            match parseCavern leftCavernString, parseCavern rightCavernString with
            //End is an end link
            | End, otherCavern | otherCavern, End ->
                otherCavern, End
            //Start is a start link
            | Start, otherCavern | otherCavern, Start ->
                Start, otherCavern
            //Other types of cavern are double linked with each-other
            | leftCavern, rightCavern ->
                leftCavern, rightCavern
                rightCavern, leftCavern
        }

    let addLinkToMap linkMap (x, y) =
        linkMap |> Map.change x (function None -> Some (y::[]) | Some list -> Some (y::list))

    System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day12", "Input.txt"))
    |> Seq.fold (fun linkMap linkString ->
        let linkStrings = linkString.Split("-")
        createLinks (linkStrings[0], linkStrings[1])
        |> Seq.fold addLinkToMap linkMap
    ) Map.empty

let part1 () =
    let linkedCaves = readInput ()

    let rec travel (currentCave: Cave, visitedSmallCaves: Set<string>, stack: Cave list) =
        match currentCave with
        | End -> [ currentCave::stack ]
        | SmallCave x when visitedSmallCaves.Contains(x) ->
            []
        | SmallCave x ->
            linkedCaves[currentCave]
            |> List.collect (fun linkedCave -> travel(linkedCave, visitedSmallCaves.Add x, currentCave::stack))
        | _other ->
            linkedCaves[currentCave]
            |> List.collect (fun x -> travel(x, visitedSmallCaves, currentCave::stack))

    let possibleTraversals = travel (Start, Set.empty, [])

    printfn "Day 12 - Part 1 - Number of possible traversals: %i" possibleTraversals.Length

let part2 () =
    let input = readInput ()

    let rec travel (currentCave: Cave, visitedSmallCaves: Map<string, int>, stack: Cave list) =
        match currentCave with
        | End -> [ List.rev (currentCave::stack) ]
        | SmallCave smallCave ->
            //If we've already visited the small cave, check if we can visit it again
            if visitedSmallCaves.ContainsKey smallCave && (visitedSmallCaves |> Map.exists (fun _smallCave nbTimesVisited -> nbTimesVisited = 2)) then
                []
            else
                input[currentCave]
                |> List.collect (fun link -> travel (link, visitedSmallCaves.Change (smallCave, (function Some nbTimesVisited -> Some (nbTimesVisited + 1) | None -> Some 1)), currentCave::stack))
        | _other ->
            input[currentCave]
            |> List.collect (fun link -> travel (link, visitedSmallCaves, currentCave::stack))

    let possibleTraversals = travel (Start, Map.empty, [])

    printfn "Day 12 - Part 2 - Number of possible traversals: %i" possibleTraversals.Length