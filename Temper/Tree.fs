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

type Expr =
    | Variable of ident: string
    | Property of Expr * prop: string
    | ObjectCast of Expr * kind: string
    // Optional data -> provide default? right now defaults to empty string
module Expr =
    let rec evaluate (ex: Expr) (vars: Vars) =
        match ex with
        | Expr.Variable v -> Map.find v vars
        | Expr.Property (ex, prop) -> (evaluate ex vars).Lens.[prop].Value
        | Expr.ObjectCast (ex, kind) ->
            match evaluate ex vars with
            | VarValue.Object (ms, _) -> VarValue.Object (ms, kind)
            | _ -> failwith "impossible, this should have been caught at sem analysis"

// Required = true => You cannot write this template without providing a value
// Required = false => There exists generic default behaviour if you don't provide a value
type VarDefinition = { Type: VarType; Required: bool }

type PatternGuts =
    | Exact of string
    | CaseInsensitive of string
    | Regex of string
    | Whitespace of Whitespaces
    | Expr of Expr
    | Choice of PatternGuts * PatternGuts
    | Optional of PatternGuts * defaultNone: bool
    | Star of PatternGuts * defaultEmpty: bool
    | Subtemplate of SubTemplate
    | Auto

and PatternInferFunc =
    | F of func: (Vars -> VarValue)
    | CannotInfer of message: string

and Pattern = 
    {
        Guts: PatternGuts
        InferDefault: PatternInferFunc // Infer func is created by sem analysis
    }

and TemplateFragment =
    | Raw of string
    | Discard of Pattern
    | Capture of ident: string * Pattern

and Template =
    {
        Body: TemplateFragment list
        Variables: Map<string, VarDefinition>
        //Parameters: Map<string, VarDefinition>
    }

and SubTemplate = Template // just for now