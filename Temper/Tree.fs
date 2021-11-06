namespace Temper.Tree

(*
    Tree layer - Contains the syntax tree for writing templates
*)

open Temper.Data

type Whitespaces =
    | NoWhitespace
    | Tab
    | Space
    | Newline
    member this.AsString
        with get() =
            match this with
            | NoWhitespace -> ""
            | Tab -> "\t"
            | Space -> " "
            | Newline -> "\n"

type PatternGuts =
    | Exact of string
    | CaseInsensitive of string
    | Regex of string
    | Whitespace of Whitespaces
    | Variable of ident: string
    | Choice of PatternGuts * PatternGuts
    | Optional of PatternGuts * defaultNone: bool
    | Star of PatternGuts * defaultEmpty: bool
    | Auto

type PatternInferFunc =
    | F of func: (Vars -> VarValue)
    | CannotInfer of message: string

type Pattern = 
    {
        Guts: PatternGuts
        InferDefault: PatternInferFunc // Infer func is created by sem analysis
    }

type TemplateFragment =
    | Raw of string
    | Discard of Pattern
    | Capture of ident: string * Pattern

type Template =
    {
        Body: TemplateFragment list
        Variables: Map<string, VarType * bool>
        Parameters: Map<string, VarType * bool>
    }