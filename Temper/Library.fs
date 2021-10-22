namespace Temper

open System.Text

type Vars = Map<string, string>

type WhitespaceDefault =
    | NoWhitespace
    | Tab
    | Space
    | Newline

module WhitespaceDefault =
    
    let toString wd =
        match wd with
        | NoWhitespace -> ""
        | Tab -> "\t"
        | Space -> " "
        | Newline -> "\n"

type MatchExt =
    | Variable of ident: string
    | Exact of string
    | CaseInsensitive of string
    | Whitespace of WhitespaceDefault
    | Regex of string
    | Auto
    | Choice of MatchExt * MatchExt

type Match =
    | Variable of ident: string
    | Exact of string
    | CaseInsensitive of string
    | Whitespace of WhitespaceDefault
    | Regex of string
    | Choice of Match * Match

module Match =
    
    let rec canInferDefault m =
        match m with
        | Variable _
        | Exact _
        | CaseInsensitive _
        | Whitespace _ -> true
        | Choice (first, rest) -> canInferDefault first || canInferDefault rest
        | Regex _ -> false

    let rec inferDefault vars m =
        match m with
        | Variable v -> Map.find v vars
        | Exact s -> s
        | CaseInsensitive s -> s
        | Whitespace wd -> WhitespaceDefault.toString wd
        | Choice (first, rest) -> if canInferDefault first then inferDefault vars first else inferDefault vars rest
        | Regex _ -> failwith "Cannot infer a default value here"

type TemplateFragmentExt =
    | Raw of string
    | Discard of MatchExt
    | Variable of ident: string * MatchExt

type TemplateFragment =
    | Raw of string
    | Discard of Match
    | Variable of ident: string * Match

type TemplateWarning =
    | Warning of fragPos: int * message: string
    | Critical of message: string

type Template =
    {
        Body: TemplateFragment list
        RequiredVariables: string list
    }

module Template =
    
    open FParsec
    open System.IO

    module Semantics =

        let check (fragments: TemplateFragmentExt array) : Result<Template * TemplateWarning list, TemplateWarning> =
            let warnings = ResizeArray<TemplateWarning>()

            let requiredVariables = ResizeArray<string>()
            let variablesExist = ResizeArray<string>()

            let rec convertMatchExt (i: int) (m: MatchExt) =
                match m with
                | MatchExt.Variable v ->
                    if not (variablesExist.Contains v) then warnings.Add (Warning (i, "This variable doesn't (yet) exist!"))
                    Match.Variable v
                | MatchExt.Exact s -> Match.Exact s
                | MatchExt.CaseInsensitive s -> Match.CaseInsensitive s
                | MatchExt.Whitespace wd -> Match.Whitespace wd
                | MatchExt.Regex expr -> Match.Regex expr
                | MatchExt.Auto ->
                    if i + 1 = fragments.Length then "[\s\S]*"
                    else
                        match fragments.[i + 1] with
                        | TemplateFragmentExt.Raw s -> sprintf @"([\s\S]+?)(?=%s)" (RegularExpressions.Regex.Escape s)
                        | _ -> 
                            warnings.Add (Warning (i, "An auto pattern must be followed directly by a raw text fragment to avoid ambiguous parsing."))
                            ".*?"
                    |> Match.Regex
                | MatchExt.Choice (head, rest) -> Match.Choice ((convertMatchExt i head), (convertMatchExt i rest))

            let checkFrag (i: int) (f: TemplateFragmentExt) =
                match f with
                | TemplateFragmentExt.Raw s -> Raw s
                | TemplateFragmentExt.Discard m ->
                    let cm = convertMatchExt i m
                    if not (Match.canInferDefault cm) then warnings.Add (Warning (i, "This pattern doesn't take a default value, so you shouldn't discard it!"))
                    TemplateFragment.Discard cm
                | TemplateFragmentExt.Variable (v, m) ->
                    if variablesExist.Contains v then warnings.Add (Warning (i, "This variable has already been used!")) else variablesExist.Add v
                    let cm = convertMatchExt i m
                    if not (Match.canInferDefault cm) then requiredVariables.Add v
                    TemplateFragment.Variable (v, cm)

            try
                let checkedFrags : TemplateFragment array = Array.zeroCreate fragments.Length
                Array.iteri (fun i f -> checkedFrags.[i] <- checkFrag i f) fragments

                Result.Ok (
                    { Body = List.ofArray checkedFrags; RequiredVariables = List.ofSeq requiredVariables },
                    List.ofSeq warnings
                )
            with exn -> Result.Error (Critical exn.Message)

    module Parser =

        let variableName = many1Chars2 (satisfy isAsciiUpper) (satisfy isAsciiLower) <?> "Variable identifier"

        let parseMatch : Parser<MatchExt, unit> =

            let stringEscape = manyChars ((noneOf "\"\\") <|> (pstring "\\\"" >>% '"') <|> (pstring "\\\\" >>% '\\'))

            let variable = variableName |>> MatchExt.Variable
            let exact = between (pchar '"') (pchar '"') stringEscape |>> MatchExt.Exact
            let caseInsensitive = between (pstring "^\"") (pchar '"') stringEscape |>> MatchExt.CaseInsensitive
            let whitespace =
                (pstring "space" >>% MatchExt.Whitespace WhitespaceDefault.Space)
                <|> (pstring "tab" >>% MatchExt.Whitespace WhitespaceDefault.Tab)
                <|> (pstring "newline" >>% MatchExt.Whitespace WhitespaceDefault.Newline)
                <|> (pstring "ws" >>% MatchExt.Whitespace WhitespaceDefault.NoWhitespace)
            let regex = between (pstring "r\"") (pchar '"') stringEscape |>> MatchExt.Regex
            let shorthands =
                (pstring "auto" >>% MatchExt.Auto)
                <|> (pstring "restofline" >>% MatchExt.Regex @".*")
                <|> (pstring "ident" >>% MatchExt.Regex @"\w+")

            let simpleMatch : Parser<MatchExt, unit> =
                choiceL
                    [
                        variable;
                        exact;
                        caseInsensitive;
                        whitespace;
                        regex;
                        shorthands
                    ] "pattern"

            let loop, loopRef = createParserForwardedToRef()
            loopRef.Value <-
                simpleMatch .>>. (opt (pchar '|' >>. loop))
                |>> fun (head, rest) ->
                    match rest with Some r -> MatchExt.Choice (head, r) | None -> head
            loop

        let parseFragment : Parser<TemplateFragmentExt, unit> =
            let raw = many1CharsTill anyChar (followedBy (pstring "{{") <|> eof) |>> TemplateFragmentExt.Raw
            let discard = between (pstring "{{:") (pstring "}}") parseMatch |>> TemplateFragmentExt.Discard
            let variable =
                between (pstring "{{") (pstring "}}")
                    (variableName .>>. (opt (pchar ':' >>. parseMatch) |>> Option.defaultValue MatchExt.Auto)) |>> TemplateFragmentExt.Variable
            choiceL
                [
                    discard;
                    variable;
                    raw
                ] "fragment"

        let parseTemplate str : Result<Template * TemplateWarning list, TemplateWarning> =
            match run (many parseFragment) str with
            | ParserResult.Success (v, _, _) -> Semantics.check (Array.ofList v)
            | ParserResult.Failure (err, _, _) -> Result.Error (Critical err)
            
    type Reader = Parser<unit, Vars>
    module Reader =
        
        let build (template: Template) =

            let rec matchParser (m: Match) : Parser<string, Vars> =
                match m with
                | Match.Variable v -> getUserState >>= (Map.find v >> pstring)
                | Match.Exact s -> pstring s
                | Match.CaseInsensitive s -> pstring s //nyi
                | Match.Whitespace _ -> skipped spaces
                | Match.Choice (head, rest) -> matchParser head <|> matchParser rest
                | Match.Regex ex -> regex ex
            
            let fragParser (frag: TemplateFragment) : Parser<unit, Vars> =
                match frag with
                | Raw s -> skipString s
                | Discard m -> matchParser m >>% ()
                | Variable (v, m) -> matchParser m >>= (fun x -> updateUserState (Map.add v x))

            let rec parser frags : Parser<unit, Vars> =
                match frags with
                | [] -> preturn ()
                | x :: xs -> fragParser x >>. parser xs
            
            parser template.Body

        let read (p: Reader) (text: string) : Result<Vars, string> =
            match runParserOnString p Map.empty "" text with
            | Success (_, vars, _) -> Result.Ok vars
            | Failure (msg, _, _) -> Result.Error msg

    type Writer = Vars -> TextWriter -> unit
    module Writer =
        
        let build (template: Template) : Writer =
            fun (vars: Vars) ->

                for v in template.RequiredVariables do
                    if not (Map.containsKey v vars) then failwithf "Variable '%s' is required." v

                let writeFrag (tw: TextWriter) (frag: TemplateFragment) =
                    match frag with
                    | Raw s -> tw.Write s
                    | Discard m -> Match.inferDefault vars m |> tw.Write
                    | Variable (v, m) -> Map.tryFind v vars |> Option.defaultWith ( fun () -> Match.inferDefault vars m ) |> tw.Write

                fun (tw: TextWriter) ->
                    List.iter (writeFrag tw) template.Body

        let toString (vars: Vars) (w: Writer) =
            use sw = new StringWriter()
            w vars sw
            sw.ToString()