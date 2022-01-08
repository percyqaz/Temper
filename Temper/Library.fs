namespace Temper

open System.IO
open FParsec
open Temper
open Temper.Data
open Temper.Tree

type Reader = Parser<unit, Vars>
module Reader =

    let private pstringAllowNewline (str: string) =
        let parts = str.Split "\n" |> List.ofArray
        let rec f xs : Parser<unit, _> =
            match xs with
            | x :: y :: xs -> skipString x .>> pchar '\n' .>> f (y :: xs)
            | x :: [] -> skipString x
            | [] -> failwith "impossible"
        f parts >>% str

    let build (template: Template) =

        let rec varParser (v: VarValue) : Parser<VarValue, Vars> =
            match v with
            | String s -> pstringAllowNewline s >>% v
            | Option None -> preturn v
            | Option (Some x) -> varParser x >>% v
            | List (x :: xs) -> varParser x .>> varParser (List xs) >>% v
            | List [] -> preturn v
            | Object (ms, kind) -> failwith "not supported"

        let rec patternParser (m: PatternGuts) (fragAhead: TemplateFragment option) : Parser<VarValue, Vars> =
            match m with
            | Expr ex -> getUserState >>= (Expr.evaluate ex >> varParser)
            | Exact s -> pstring s |>> String
            | CaseInsensitive s -> pstringCI s |>> String
            | Whitespace _ -> skipped spaces |>> String
            | Choice (head, rest) -> patternParser head fragAhead <|> patternParser rest fragAhead
            | Regex ex -> regex ex |>> String
            | Optional (pat, _) -> opt (patternParser pat fragAhead) |>> Option
            | Star (pat, _) -> many (patternParser pat fragAhead) |>> List
            | Auto ->
                match fragAhead with
                | Some f -> (manyCharsTill anyChar (followedBy (fragParser f None)))
                | None -> manyCharsTill anyChar eof
                |>> String
            
        and fragParser (frag: TemplateFragment) (fragAhead: TemplateFragment option) : Parser<unit, Vars> =
            match frag with
            | Raw s -> pstringAllowNewline s >>% ()
            | Discard m -> patternParser m.Guts fragAhead >>% ()
            | Capture (v, m) -> patternParser m.Guts fragAhead >>= (fun x -> updateUserState (Map.add v x))

        let rec parser frags : Parser<unit, Vars> =
            match frags with
            | [] -> preturn ()
            | x :: y :: xs -> fragParser x (Some y) >>. parser (y :: xs)
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

            let rec writeValue (tw: TextWriter) (value: VarValue) =
                match value with
                | String s -> tw.Write s
                | Option None -> ()
                | Option (Some v) -> writeValue tw v
                | List xs -> List.iter (writeValue tw) xs
                | Object _ -> failwith "not supported"

            let writeFrag (tw: TextWriter) (frag: TemplateFragment) =
                match frag with
                | Raw s -> tw.Write s
                | Discard m ->
                    match m.InferDefault with
                    | F f -> f vars |> tw.Write
                    | CannotInfer reason -> failwith reason
                | Capture (v, m) ->
                    Map.tryFind v vars
                    |> Option.defaultWith
                        ( fun () -> 
                            match m.InferDefault with
                            | F f -> f vars
                            | CannotInfer reason -> failwith reason
                        )
                    |> writeValue tw

            fun (tw: TextWriter) ->
                List.iter (writeFrag tw) template.Body

    let toString (vars: Vars) (w: Writer) =
        use sw = new StringWriter()
        w vars sw
        sw.ToString()

type TemplateCreationResult =
    | Ok of Template
    | Warnings of Template * TemplateWarning list
    | SemanticFail of TemplateError
    | ParseFail of string

module Template =

    let fromString str =
        match Parser.parseFragments str with
        | Result.Ok frags ->
            match Semantics.check frags with
            | Result.Ok (tmp, []) -> Ok tmp
            | Result.Ok (tmp, warnings) -> Warnings (tmp, warnings)
            | Result.Error err -> SemanticFail err
        | Result.Error msg -> ParseFail msg