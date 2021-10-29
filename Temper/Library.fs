namespace Temper

open System.IO
open FParsec
open Temper
open Temper.Data
open Temper.Tree

type Reader = Parser<unit, Vars>
module Reader =

    let private pstringAllowNewline (str: string) =
        let parts = str.Split("\n") |> List.ofArray
        let rec f xs : Parser<unit, _> =
            match xs with
            | x :: y :: xs -> skipString x .>> pchar '\n' .>> f (y :: xs)
            | x :: [] -> skipString x
            | [] -> failwith "impossible"
        f parts >>% str

    let build (template: Template) =

        let rec matchParser (m: Match) (fragAhead: TemplateFragment option) : Parser<string, Vars> =
            match m with
            | Match.Variable v -> getUserState >>= (Map.find v >> (fun (x: VarValue) -> x.Lens.Text) >> pstringAllowNewline)
            | Match.Exact s -> pstring s
            | Match.CaseInsensitive s -> pstringCI s
            | Match.Whitespace _ -> skipped spaces
            | Match.Choice (head, rest) -> matchParser head fragAhead <|> matchParser rest fragAhead
            | Match.Regex ex -> regex ex
            | Match.Auto ->
                match fragAhead with
                | Some f -> (manyCharsTill anyChar (followedBy (fragParser f None)))
                | None -> manyCharsTill anyChar eof
            
        and fragParser (frag: TemplateFragment) (fragAhead: TemplateFragment option) : Parser<unit, Vars> =
            match frag with
            | Raw s -> pstringAllowNewline s >>% ()
            | Discard m -> matchParser m fragAhead >>% ()
            | Variable (v, m) -> matchParser m fragAhead >>= (fun x -> updateUserState (Map.add v (String x)))

        let rec parser frags : Parser<unit, Vars> =
            match frags with
            | [] -> preturn ()
            | x :: y :: xs -> fragParser x (Some y) >>. parser xs
            | x :: xs -> fragParser x None >>. parser xs
            
        parser template.Body

    let read (p: Reader) (text: string) : Result<Vars, string> =
        match runParserOnString p Map.empty "" text with
        | Success (_, vars, _) -> Result.Ok vars
        | Failure (msg, _, _) -> Result.Error msg

type Writer = Vars -> TextWriter -> unit
module Writer =
        
    let build (template: Template) : Writer =
        fun (vars: Vars) ->

            for v in template.Variables.Keys do
                if snd template.Variables.[v] && not (Map.containsKey v vars) then failwithf "Variable '%s' is required." v

            let writeFrag (tw: TextWriter) (frag: TemplateFragment) =
                match frag with
                | Raw s -> tw.Write s
                | Discard m -> Match.inferDefault vars m |> tw.Write
                | Variable (v, m) -> Map.tryFind v vars |> Option.map (fun x -> x.Lens.Text) |> Option.defaultWith ( fun () -> Match.inferDefault vars m ) |> tw.Write

            fun (tw: TextWriter) ->
                List.iter (writeFrag tw) template.Body

    let toString (vars: Vars) (w: Writer) =
        use sw = new StringWriter()
        w vars sw
        sw.ToString()

type TemplateCreationResult =
    | Ok of Template
    | Warnings of Template * TemplateWarning list
    | SemanticFail of TemplateWarning
    | ParseFail of string

module Template =

    let fromString str =
        match Parser.parseFragments str with
        | Result.Ok frags ->
            match Semantics.check frags with
            | Result.Ok (tmp, []) -> Ok tmp
            | Result.Ok (tmp, warnings) -> Warnings (tmp, warnings)
            | Result.Error msg -> SemanticFail msg
        | Result.Error msg -> ParseFail msg