/// https://adventofcode.com/2021/day/2
module Day2
open System

type Direction =
    | Forward of int
    | Up of int
    | Down of int

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (Int32.Parse (s.Substring(p.Length).Trim()))
    else
        None

let private readInput () =
    let parseDirection (str: string) =
        match str with
        | Prefix "forward" number -> Forward number
        | Prefix "up" number -> Up number
        | Prefix "down" number -> Down number
        | _ -> failwithf "Unknown direction."

    System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day2", "Input.txt"))
    |> List.ofSeq
    |> List.map parseDirection

let part1 () =
    let horizontalPosition, verticalPosition =
        readInput ()
        |> List.fold (fun (h, v) direction ->
            match direction with
            | Up x -> (h, v-x)
            | Down x -> (h, v+x)
            | Forward x -> (h+x, v)
        ) (0, 0)

    printfn "Day 2 - Part 1 - multiplied: %i" (horizontalPosition*verticalPosition)

let part2 () =
    let horizontalPosition, verticalPosition, _ =
        readInput ()
        |> List.fold (fun (h, v, aim) direction ->
            match direction with
            | Up x -> (h, v, aim - x)
            | Down x -> (h, v, aim + x)
            | Forward x -> (h + x, v + x*aim, aim)
        ) (0, 0, 0)

    printfn "Day 2 - Part 2 - multiplied: %i" (horizontalPosition*verticalPosition)