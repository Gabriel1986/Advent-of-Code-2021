/// https://adventofcode.com/2021/day/10
module Day10

let private readInput () = seq { for line in System.IO.File.ReadLines(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Day10", "Input.txt")) do yield line }

let part1 () =
    let input = readInput ()

    let validClosingMap = Map [
        '[', ']'
        '<', '>'
        '(', ')'
        '{', '}'
    ]

    let pointMap = Map [
        ')', 3
        ']', 57
        '}', 1197
        '>', 25137
    ]

    let findSyntaxError (line: string) =
        line
        |> Seq.fold (fun (stackOfOpenCharacters: char list, syntaxError: int option) (nextCharacter: char) ->
            match syntaxError with
            | Some syntaxError -> (stackOfOpenCharacters, Some syntaxError)
            | None ->
                if validClosingMap.ContainsKey nextCharacter then
                    (nextCharacter::stackOfOpenCharacters, syntaxError)
                else
                    match stackOfOpenCharacters with
                    | [] ->
                        (stackOfOpenCharacters, Some (pointMap[nextCharacter]))
                    | head::tail when validClosingMap[head] = nextCharacter ->
                        (tail, None)
                    | _ ->
                        ([], Some (pointMap[nextCharacter]))) ([], None)
        |> snd

    printfn "Day 10 - Part 1 - Total syntax error score: %i" (input |> Seq.choose findSyntaxError |> Seq.sum)

let part2 () =
    let input = readInput ()

    let validClosingMap = Map [
        '[', ']'
        '<', '>'
        '(', ')'
        '{', '}'
    ]

    let pointMap = Map [
        ')', 1L
        ']', 2L
        '}', 3L
        '>', 4L
    ]

    let findOpenCharacters (line: string) =
        line
        |> Seq.fold (fun (stackOfOpenCharacters: char list, hasSyntaxError: bool) (nextCharacter: char) ->
            match hasSyntaxError with
            | true -> (stackOfOpenCharacters, true)
            | false ->
                if validClosingMap.ContainsKey nextCharacter then
                    (nextCharacter::stackOfOpenCharacters, false)
                else
                    match stackOfOpenCharacters with
                    | [] ->
                        (stackOfOpenCharacters, true)
                    | head::tail when validClosingMap[head] = nextCharacter ->
                        (tail, false)
                    | _ ->
                        ([], true)) ([], false)
        |> (fun (openCharacters: char list, syntaxError: bool) ->
            match syntaxError with
            | true -> None
            | false -> Some openCharacters
        )

    let scores =
        input
        |> Seq.choose (findOpenCharacters >> Option.map (List.fold (fun totalScore character -> 5L * totalScore + pointMap[validClosingMap[character]]) 0L))

    printfn "Day 10 - Part 2 - Total invalid score: %i" (scores |> Seq.sort |> Seq.item (Seq.length scores / 2))