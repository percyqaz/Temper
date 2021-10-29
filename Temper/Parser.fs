namespace Temper

(*
    Parse layer - Parses a template to intermediary parse tree for semantic analysis
*)

open FParsec
open Temper.Tree

type MatchExt =
    | Variable of ident: string
    | Exact of string
    | CaseInsensitive of string
    | Whitespace of WhitespaceDefault
    | Regex of string
    | Auto
    | Choice of MatchExt * MatchExt

type TemplateFragmentExt =
    | Raw of string
    | Discard of MatchExt
    | Variable of ident: string * MatchExt

module Parser =

    let variableName = many1Chars2 (satisfy isAsciiUpper) (satisfy isLetter) <?> "Variable identifier"

    let parseMatch : Parser<MatchExt, unit> =

        let stringEscape = manyChars ((noneOf "\"\\\r\n") <|> (pstring "\\\"" >>% '"') <|> (pstring "\\\\" >>% '\\'))

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
        let raw = many1CharsTill anyChar (followedBy (pstring "%") <|> eof) |>> TemplateFragmentExt.Raw
        let discard = between (pstring "%:") (pstring "%") parseMatch |>> TemplateFragmentExt.Discard
        let variable =
            between (pstring "%") (pstring "%")
                (variableName .>>. (opt (pchar ':' >>. parseMatch) |>> Option.defaultValue MatchExt.Auto)) |>> TemplateFragmentExt.Variable
        choiceL
            [
                discard;
                variable;
                raw
            ] "fragment"

    let parseFragments str : Result<TemplateFragmentExt array, string> =
        match run (many parseFragment) str with
        | ParserResult.Success (v, _, _) -> Result.Ok (Array.ofList v)
        | ParserResult.Failure (err, _, _) -> Result.Error err