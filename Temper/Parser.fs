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
    | Variable of ident: string
    | Choice of PatternEx * PatternEx
    | Optional of PatternEx
    | Star of PatternEx
    | Definition of ident: string
    | Auto

type TemplateFragmentEx =
    | Comment of string
    | Raw of string
    | Discard of PatternEx
    | Capture of ident: string * PatternEx
    | Define of ident: string * PatternEx

module Parser =

    let variableName = many1Chars2 (satisfy isAsciiUpper) (satisfy isLetter) <?> "Variable identifier"

    let parsePattern : Parser<PatternEx, unit> =

        let stringEscape = manyChars ((noneOf "\"\\\r\n") <|> (pstring "\\\"" >>% '"') <|> (pstring "\\\\" >>% '\\'))

        let variable = variableName |>> Variable // todo: 'as' pattern
        let definition = pchar '#' >>. variableName |>> Definition
        let exact = between (pchar '"') (pchar '"') stringEscape |>> Exact
        let caseInsensitive = between (pstring "^\"") (pchar '"') stringEscape |>> CaseInsensitive
        let whitespace =
            (pstring "space" >>% Whitespace Space)
            <|> (pstring "tab" >>% Whitespace Tab)
            <|> (pstring "newline" >>% Whitespace Newline)
            <|> (pstring "ws" >>% Whitespace NoWhitespace)
        let regex = between (pstring "r\"") (pchar '"') stringEscape |>> Regex
        let shorthands =
            (pstring "auto" >>% Auto)
            <|> (pstring "restofline" >>% Regex @".*")
            <|> (pstring "ident" >>% Regex @"\w+")

        let pattern, patternRef = createParserForwardedToRef()

        let simplePattern : Parser<PatternEx, unit> =
            choiceL
                [
                    caseInsensitive
                    regex
                    exact
                    definition
                    variable
                    shorthands
                    whitespace
                ] "pattern"

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

    let parseFragment : Parser<TemplateFragmentEx, unit> =

        let raw = 
            many1CharsTill
                anyChar
                (followedBy (pstring "%") <|> eof)
             |>> Raw

        let discard =
            between
                (pstring "%:" .>> spaces)
                (spaces >>. pstring "%")
                parsePattern
            |>> Discard

        let variable =
            between
                (pstring "%" >>. spaces)
                (spaces >>. pstring "%")
                (variableName .>>. (opt (pchar ':' >>. spaces >>. parsePattern) |>> Option.defaultValue Auto))
            |>> Capture

        let comment =
            between
                (pstring "%*")
                (pstring "*%")
                (manyCharsTill anyChar (followedBy (pstring "*%")))
            |>> Comment

        let definition =
            between
                (pstring "%#" >>. spaces)
                (spaces >>. pstring "%")
                (variableName .>>. (spaces >>. pchar '=' >>. spaces >>. parsePattern))
            |>> Define

        choiceL
            [
                comment
                definition
                discard
                variable
                raw
            ] "fragment"

    let parseFragments str : Result<TemplateFragmentEx array, string> =
        match run (many parseFragment) str with
        | ParserResult.Success (v, _, _) -> Result.Ok (Array.ofList v)
        | ParserResult.Failure (err, _, _) -> Result.Error err