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
    | Auto

type TemplateFragmentEx =
    | Raw of string
    | Discard of PatternEx
    | Capture of ident: string * PatternEx

module Parser =

    let variableName = many1Chars2 (satisfy isAsciiUpper) (satisfy isLetter) <?> "Variable identifier"

    let parseMatch : Parser<PatternEx, unit> =

        let stringEscape = manyChars ((noneOf "\"\\\r\n") <|> (pstring "\\\"" >>% '"') <|> (pstring "\\\\" >>% '\\'))

        let variable = variableName |>> Variable
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

        let simpleMatch : Parser<PatternEx, unit> =
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
                match rest with Some r -> Choice (head, r) | None -> head
        loop

    let parseFragment : Parser<TemplateFragmentEx, unit> =
        let raw = many1CharsTill anyChar (followedBy (pstring "%") <|> eof) |>> Raw
        let discard = between (pstring "%:") (pstring "%") parseMatch |>> Discard
        let variable =
            between (pstring "%") (pstring "%")
                (variableName .>>. (opt (pchar ':' >>. parseMatch) |>> Option.defaultValue Auto)) |>> Capture
        choiceL
            [
                discard;
                variable;
                raw
            ] "fragment"

    let parseFragments str : Result<TemplateFragmentEx array, string> =
        match run (many parseFragment) str with
        | ParserResult.Success (v, _, _) -> Result.Ok (Array.ofList v)
        | ParserResult.Failure (err, _, _) -> Result.Error err