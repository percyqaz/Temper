namespace Temper

(*
    Parse layer - Parses a template to intermediary parse tree for semantic analysis
*)

open FParsec
open Temper.Tree

module Parser =

    let debug label p = p |>> (fun x -> printfn "Parsed %s: %A" label x; x)

    let variableName = many1Chars2 (satisfy isAsciiUpper) (satisfy isLetter) <?> "Variable identifier"
    
    let parseSubtemplate, parseSubtemplateRef = createParserForwardedToRef()

    let parsePattern : Parser<Pattern, unit> =

        let stringEscape = manyChars ((noneOf "\"\\\r\n") <|> (pstring "\\\"" >>% '"') <|> (pstring "\\\\" >>% '\\'))

        let caseInsensitive = between (pstring "^\"") (pchar '"') stringEscape |>> Pattern.CaseInsensitive
        let regex = between (pstring "r\"") (pchar '"') stringEscape |>> Pattern.Regex
        let exact = between (pchar '"') (pchar '"') stringEscape |>> Pattern.Exact
        let definition = pchar '#' >>. variableName |>> Pattern.Macro
        let subtemplate = between (pstring "<#") (pstring "#>") parseSubtemplate |>> Pattern.Subtemplate
        let variable = variableName |>> (Expr.Var >> Pattern.Expr)
        let shorthands =
            (pstring "auto" >>% Pattern.Auto)
            // todo: move these to built-in macros
            //<|> (pstring "restofline" >>% Pattern.Regex @".*")
            //<|> (pstring "ident" >>% Pattern.Regex @"\w+")
        let whitespace =
            (pstring "space" >>% Pattern.Whitespace Whitespace.Space)
            <|> (pstring "tab" >>% Pattern.Whitespace Whitespace.Tab)
            <|> (pstring "nl" >>% Pattern.Whitespace Whitespace.Newline)
            <|> (pstring "ws" >>% Pattern.Whitespace Whitespace.None)

        let pattern, patternRef = createParserForwardedToRef()

        let simplePattern : Parser<Pattern, unit> =
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

        let specialPattern : Parser<Pattern, unit> =
            between (pchar '(' .>> spaces) (spaces >>. pchar ')') pattern <|> simplePattern >>=
            ( fun pat ->
                (pchar '*' >>% Pattern.Star pat)
                //<|> (pchar '?' >>% Optional pat)
                <|> preturn pat
            )

        patternRef.Value <-
            specialPattern
            .>>. (opt (attempt (spaces >>. pchar '|' >>. spaces) >>. pattern))
            |>> function (head, Some rest) -> Pattern.Choice (head, rest) | (head, None) -> head

        pattern

    let parseFragment (isSubtemplate: bool) : Parser<Frag, unit> =

        // Tags & plain text fragments

        let open_tag inner = 
            pstring ("%-" + inner)
            <|> pstring ("%" + inner)
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
            |>> Frag.Pure

        // Tag types

        let discard =
            between
                (open_tag ":" .>> spaces)
                (spaces >>. close_tag "")
                parsePattern
            |>> Frag.Discard

        let variable =
            between
                (open_tag "" >>. spaces)
                (spaces >>. close_tag "")
                (variableName .>>. (opt (pchar ':' >>. spaces >>. parsePattern) |>> Option.defaultValue Pattern.Auto))
            |>> Frag.Capture

        let comment =
            between
                (open_tag "*")
                (close_tag "*")
                (manyCharsTill anyChar (followedBy (close_tag "*")))
            |>> Frag.Comment

        let definition =
            between
                (open_tag "#" >>. spaces)
                (spaces >>. close_tag "")
                (variableName .>>. (spaces >>. pchar '=' >>. spaces >>. parsePattern))
            |>> Frag.Define_Macro

        choiceL
            [
                comment
                definition
                discard
                variable
                raw
            ] "Fragment"

    do parseSubtemplateRef.Value <- manyTill (parseFragment true) (followedBy (pstring "#>")) |>> fun fs -> { Body = fs }
    let parseTemplate = many (parseFragment false) .>> eof

    let parseFragments str : Result<Frag list, string> =
        match run parseTemplate str with
        | ParserResult.Success (v, _, _) -> Result.Ok v
        | ParserResult.Failure (err, _, _) -> Result.Error err