module Program
open System
open System.Reflection

let private allPrograms =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Array.collect (fun typ ->
        typ.GetMethods()
        |> Array.choose (fun each ->
            if typ.Name.StartsWith("Day") && each.Name.StartsWith "part" then
                Some ((Int32.Parse(typ.Name.Replace("Day", "")), Int32.Parse(each.Name.Replace("part", ""))), (fun () -> each.Invoke (typ, [||]) |> ignore))
            else
                None
        )
    )
    |> Map.ofArray

let executeSpecificPart (day, part) =
    match allPrograms.TryFind ((Int32.Parse day, Int32.Parse part)) with
    | None ->
        failwith $"Function on day %s{day}, part %s{part} not found.."
    | Some fn ->
        fn ()

let executeDay (runForDay) =
    allPrograms
    |> Map.toList
    |> List.filter (fun ((day, _), _) -> string day = runForDay)
    |> List.sortBy (fun ((day, part), _) -> day * 10 + part)
    |> List.iter (fun (_, fn) -> fn ())

let executeLatest () =
    allPrograms
    |> Map.toList
    |> List.maxBy (fun ((day, part), _) -> day * 10 + part)
    |> (fun (_, fn) -> fn ())

let executeAll () =
    allPrograms
    |> Map.toList
    |> List.sortBy (fun ((day, part), _) -> day * 10 + part)
    |> List.iter (fun (_, fn) -> fn ())

[<EntryPoint>]
let main args =
    printfn "----------------------------------------------------------------------------"
    printfn "'dotnet run' for the latest solution [default]"
    printfn "'dotnet run all' to run all of the solutions"
    printfn "'dotnet run <day>' for running the solution for all parts for a specific day"
    printfn "'dotnet run <day> <part>' to run the solution for the specific day and part"
    printfn "----------------------------------------------------------------------------"
    match args with
    | [| "all" |] | [| "All" |] ->
        printfn "Running solution for all"
        printfn ""
        executeAll ()
    | [| day |] ->
        printfn "Running solution for day '%s'" day
        printfn ""
        executeDay (day)
    | [| day; part |] ->
        printfn "Running solution for day '%s', part '%s'" day part
        printfn ""
        executeSpecificPart (day, part)
    | other ->
        printfn "Running latest solution..."
        printfn ""
        executeLatest ()
    0