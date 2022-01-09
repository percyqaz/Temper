namespace Temper

(*
    Parse layer - Parses a template to intermediary parse tree for semantic analysis
*)

open FParsec
open Temper.Tree

type PatternEx =
    | Exact of string
    | CaseInsensitive of string
    | Regex of string
    | Whitespace of Whitespaces
    | Expr of Expr
    | Choice of PatternEx * PatternEx
    | Optional of PatternEx
    | Star of PatternEx
    | Definition of ident: string
    | Subtemplate of TemplateFragmentEx list
    | Auto

and TemplateFragmentEx =
    | Comment of string
    | Raw of string
    | Discard of PatternEx
    | Capture of ident: string * PatternEx
    | Define of ident: string * PatternEx

module Parser =

    let debug label p = p |>> (fun x -> printfn "Parsed %s: %A" label x; x)

    let variableName = many1Chars2 (satisfy isAsciiUpper) (satisfy isLetter) <?> "Variable identifier"
    
    let parseSubtemplate, parseSubtemplateRef = createParserForwardedToRef()

    let parsePattern : Parser<PatternEx, unit> =

        let stringEscape = manyChars ((noneOf "\"\\\r\n") <|> (pstring "\\\"" >>% '"') <|> (pstring "\\\\" >>% '\\'))

        let caseInsensitive = between (pstring "^\"") (pchar '"') stringEscape |>> CaseInsensitive
        let regex = between (pstring "r\"") (pchar '"') stringEscape |>> Regex
        let exact = between (pchar '"') (pchar '"') stringEscape |>> Exact
        let definition = pchar '#' >>. variableName |>> Definition
        let subtemplate = between (pstring "<#") (pstring "#>") parseSubtemplate |>> Subtemplate
        let variable = variableName |>> (Variable >> Expr) // todo: 'as' pattern
        let shorthands =
            (pstring "auto" >>% Auto)
            <|> (pstring "restofline" >>% Regex @".*")
            <|> (pstring "ident" >>% Regex @"\w+")
        let whitespace =
            (pstring "space" >>% Whitespace Space)
            <|> (pstring "tab" >>% Whitespace Tab)
            <|> (pstring "newline" >>% Whitespace Newline)
            <|> (pstring "ws" >>% Whitespace NoWhitespace)

        let pattern, patternRef = createParserForwardedToRef()

        let simplePattern : Parser<PatternEx, unit> =
            choiceL
                [
                    caseInsensitive
                    regex
                    exact
                    definition
                    subtemplate
                    variable
                    shorthands
                    whitespace
                ] "Pattern"

        let specialPattern : Parser<PatternEx, unit> =
            between (pchar '(' .>> spaces) (spaces >>. pchar ')') pattern <|> simplePattern >>=
            ( fun pat ->
                (pchar '?' >>% Optional pat)
                <|> (pchar '*' >>% Star pat)
                <|> preturn pat
            )

        patternRef.Value <-
            specialPattern
            .>>. (opt (attempt (spaces >>. pchar '|' >>. spaces) >>. pattern))
            |>> function (head, Some rest) -> Choice (head, rest) | (head, None) -> head

        pattern

    let parseFragment (isSubtemplate: bool) : Parser<TemplateFragmentEx, unit> =

        // Tags & plain text fragments

        let open_tag inner = 
            pstring ("%-" + inner)
            <|>pstring ("%" + inner)
            <?> "Opening tag"

        let close_tag inner = 
            (pstring (inner + "-%") .>> spaces)
            <|> pstring (inner + "%")
            <?> "Closing tag"
            
        let raw = 
            attempt (spaces >>. followedBy (pstring "%-")) >>% ""
            <|>
            many1CharsTill
                anyChar
                (
                    attempt (spaces >>. followedBy (pstring "%-"))
                    <|> followedBy (pstring "%")
                    <|> (if isSubtemplate then followedBy (pstring "#>") else eof)
                )
            |>> Raw

        // Tag types

        let discard =
            between
                (open_tag ":" .>> spaces)
                (spaces >>. close_tag "")
                parsePattern
            |>> Discard

        let variable =
            between
                (open_tag "" >>. spaces)
                (spaces >>. close_tag "")
                (variableName .>>. (opt (pchar ':' >>. spaces >>. parsePattern) |>> Option.defaultValue Auto))
            |>> Capture

        let comment =
            between
                (open_tag "*")
                (close_tag "*")
                (manyCharsTill anyChar (followedBy (close_tag "*")))
            |>> Comment

        let definition =
            between
                (open_tag "#" >>. spaces)
                (spaces >>. close_tag "")
                (variableName .>>. (spaces >>. pchar '=' >>. spaces >>. parsePattern))
            |>> Define

        choiceL
            [
                comment
                definition
                discard
                variable
                raw
            ] "Fragment"

    do parseSubtemplateRef.Value <- manyTill (parseFragment true) (followedBy (pstring "#>"))
    let parseTemplate = many (parseFragment false)

    let parseFragments str : Result<TemplateFragmentEx array, string> =
        match run (parseTemplate) str with
        | ParserResult.Success (v, _, _) -> Result.Ok (Array.ofList v)
        | ParserResult.Failure (err, _, _) -> Result.Error err