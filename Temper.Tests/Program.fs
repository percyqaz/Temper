module Program =

    open Temper
    open System
    open FParsec

    [<EntryPoint>]
    let main _ =
        while true do
            printfn "Enter template"
            match Template.fromString (Console.ReadLine()) with
            | TemplateCreationResult.Ok template ->
                let p = Reader.build template
                let w = Writer.build template
                let mutable line = Console.ReadLine()
                while line <> "" do
                    match runParserOnString p Map.empty "" line with
                    | Success (_, vars, _) ->
                        printfn "%A" vars
                        printfn "%s" (Writer.toString vars w)
                    | Failure (err, _, _) -> printfn "%s" err
                    line <- Console.ReadLine()
            | w -> printfn "%A" w
        0
