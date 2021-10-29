namespace Temper.Tree

(*
    Tree layer - Contains the syntax tree for writing templates
*)

open Temper.Data

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

type Match =
    | Variable of ident: string
    | Exact of string
    | CaseInsensitive of string
    | Whitespace of WhitespaceDefault
    | Regex of string
    | Choice of Match * Match
    | Auto

module Match =
    
    let rec canInferDefault m =
        match m with
        | Variable _
        | Exact _
        | CaseInsensitive _
        | Whitespace _ -> true
        | Choice (first, rest) -> canInferDefault first || canInferDefault rest
        | Auto
        | Regex _ -> false

    let rec inferDefault vars m =
        match m with
        | Variable v -> Map.find v vars |> fun (x: VarValue) -> x.Lens.Text
        | Exact s -> s
        | CaseInsensitive s -> s
        | Whitespace wd -> WhitespaceDefault.toString wd
        | Choice (first, rest) -> if canInferDefault first then inferDefault vars first else inferDefault vars rest
        | Auto
        | Regex _ -> failwith "Cannot infer a default value here"

type TemplateFragment =
    | Raw of string
    | Discard of Match
    | Variable of ident: string * Match

type Template =
    {
        Body: TemplateFragment list
        Variables: Map<string, VarType * bool>
        Parameters: Map<string, VarType * bool>
    }