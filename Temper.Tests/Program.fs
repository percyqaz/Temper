module Program =

    open Temper
    open System
    open FParsec

    [<EntryPoint>]
    let main _ =
        while true do
            printfn "Enter template"
            match Template.Parser.parseTemplate (Console.ReadLine()) with
            | Result.Ok (template, warnings) ->
                printfn "%A" warnings
                let p = Template.Reader.build template
                let w = Template.Writer.build template
                let mutable line = Console.ReadLine()
                while line <> "" do
                    match runParserOnString p Map.empty "" line with
                    | Success (_, vars, _) ->
                        printfn "%A" vars
                        printfn "%s" (Template.Writer.toString vars w)
                    | Failure (err, _, _) -> printfn "%s" err
                    line <- Console.ReadLine()
            | Result.Error w -> printfn "%A" w
        0
