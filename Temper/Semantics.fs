namespace OLD

(*
    Semantic check layer - Verifies the meaning & validity of a template before it can be used.
*)

open System.Collections.Generic
open OLD.Data
open OLD.Tree

module WarningCode =
    module Variable =
        let notFound = "TM001"
        let alreadyExists = "TM002"
    module Pattern =
        let consecutiveAuto = "TM101"
        let discardingWithoutDefault = "TM102"
        let typeInferFailed = "TM103"
        let optionInChoice = "TM104"
        let ambiguousList = "TM105"
        let ambiguous = "TM106"
    module PatternDefinition =
        let notFound = "TM201"
        let alreadyExists = "TM202"
        

open WarningCode

type TemplateWarning = Warning of fragPos: int * code: string * message: string

exception TemplateError of fragPos: int * code: string * message: string

type TemplateContext =
    | Global
    | Inner of unit // subtemplates aren't parameterised by the outer template yet, and may never be

module Semantics =
    // need to check:
    // variables are not used before they are captured YEP
    // parameters are not used if they are not defined NYI
    // two auto fragments do not appear next to each other YEP
    
    // need to do:
    // infer types for variables and parameters NYI
    // warn & remove uninferred/unused parameters NYI
    // analyse capture patterns for ambiguity/redundancy NYI
    // analyse anonymous patterns for defaultability YEP

    let check (ctx: TemplateContext) (fragments: TemplateFragmentEx array) : Result<Template * TemplateWarning list, TemplateError> =

        let warnings = ResizeArray<TemplateWarning>()
        let warn i code msg = warnings.Add (Warning (i, code, msg))
        let crit i code msg = raise (TemplateError (i, code, msg))

        let varDefs = Dictionary<string, VarDefinition>()
        let patternDefs = Dictionary<string, PatternGuts>()

        // Works out if a pattern can generate a default value when writing
        // Used when writing discard fragments
        let rec inferDefaultFunc (m: PatternGuts) : PatternInferFunc =
            match m with
            | Exact s -> F (fun _ -> String s)
            | CaseInsensitive s -> F (fun _ -> String s)
            | Whitespace wd -> F (fun _ -> String wd.AsString)
            | Optional (pat, defaultNone) -> 
                if defaultNone then 
                    F (fun _ -> Option None)
                else 
                    match inferDefaultFunc pat with
                    | F inner -> F (fun vars -> inner vars |> Some |> Option)
                    | err -> err
            | Star (pat, defaultEmpty) -> 
                if defaultEmpty then 
                    F (fun _ -> List [])
                else 
                    match inferDefaultFunc pat with
                    | F inner -> F (fun vars -> inner vars |> List.singleton |> List)
                    | err -> err
            | Expr ex -> F (Expr.evaluate ex)
            | Choice (first, rest) ->
                match inferDefaultFunc first with
                | F f -> F f
                | CannotInfer reason -> inferDefaultFunc rest
            | Auto -> CannotInfer "Auto-fragments don't have a default write value"
            | Regex _ -> CannotInfer "Regex expressions don't have a default write value"
            | Subtemplate _ -> failwith "not supported"

        // Works out the type of a pattern (inability to determine this causes semantic analysis to fail)
        let rec inferPatternType (i: int) (m: PatternGuts) : VarType =
            match m with
            | Exact _ 
            | CaseInsensitive _
            | Whitespace _ 
            | Regex _
            | Auto -> VarType.String
            | Optional (pat, _) -> VarType.Option (inferPatternType i pat)
            | Star (pat, _) -> VarType.List (inferPatternType i pat)
            | Expr ex ->
                let rec checkExpr ex =
                    match ex with
                    | Expr.Variable v -> 
                        if varDefs.ContainsKey v then varDefs.[v].Type
                        else crit i Variable.notFound (sprintf "The variable '%s' doesn't exist!" v)
                    | Expr.Property (ex, prop) ->
                        match checkExpr ex with
                        | VarType.Object ms ->
                            if Map.containsKey prop ms then ms.[prop]
                            else crit i Variable.notFound (sprintf "The property '%s' doesn't exist for this value!" prop)
                        | _ -> crit i Variable.notFound (sprintf "This value is not an object, and is not subscriptable!")
                    | Expr.ObjectCast _ -> failwith "nyi"
                checkExpr ex
            | Choice (first, rest) ->
                let firstType = inferPatternType i first
                let restType = inferPatternType i rest
                match Var.unifyTypes firstType restType with
                | Var.TypeCheckResult.Ok ty -> ty
                | err ->
                    let msg = Var.TypeCheckResult.prettyPrint err
                    crit i Pattern.typeInferFailed (sprintf "Failed to infer type for this fragment:\n%s" msg)
            | Subtemplate _ -> failwith "not supported"

        // Converts PatternEx to PatternGuts
        // Allows for syntactic sugar/macro constructs that are converted to simpler internal representation
        // This is also where we check patterns for sensibility
        let mutable blockAutoFrag = -1
        let rec checkPatternEx (i: int) (m: PatternEx) : PatternGuts =
            match m with
            | PatternEx.Expr v -> Expr v
            | PatternEx.Exact s -> Exact s
            | PatternEx.CaseInsensitive s -> CaseInsensitive s
            | PatternEx.Whitespace wd -> Whitespace wd
            | PatternEx.Regex expr -> Regex expr
            | PatternEx.Auto ->
                if blockAutoFrag = i then
                    warn i Pattern.consecutiveAuto "An auto pattern cannot be followed directly by another auto pattern to avoid ambiguous parsing."
                blockAutoFrag <- i + 1
                Auto
            | PatternEx.Definition d ->
                if patternDefs.ContainsKey d then patternDefs.[d] else crit i PatternDefinition.notFound (sprintf "The pattern macro '%s' doesn't exist!" d)
            | PatternEx.Choice (head, rest) ->
                match head, rest with
                | _, PatternEx.Optional _
                | PatternEx.Optional _, _ -> warn i Pattern.optionInChoice "Optional patterns cannot appear in choices" // but they can appear last in cases if/when those get added
                | _ -> ()
                Choice ((checkPatternEx i head), (checkPatternEx i rest))
            | PatternEx.Optional pat ->
                match pat with
                | PatternEx.Optional _ -> warn i Pattern.ambiguous "Optional-optional value is ambiguous"
                | PatternEx.Auto _ -> warn i Pattern.ambiguous "Auto-pattern need not be optional"
                | _ -> ()
                Optional (checkPatternEx i pat, true) // todo: user choice if option should try to use default of inner pattern instead of none
            | PatternEx.Star pat ->
                match pat with
                | PatternEx.Star _ -> warn i Pattern.ambiguous "List-of-lists is ambiguous in this context"
                | PatternEx.Optional _ -> warn i Pattern.ambiguous "List-of-options is ambiguous"
                | PatternEx.Auto _ -> warn i Pattern.ambiguous "Auto fragment cannot appear inside list pattern"
                | _ -> ()
                Star (checkPatternEx i pat, true) // todo: user choice if list should try to use singleton of inner pattern default instead of empty
            | PatternEx.Subtemplate _ ->
                // todo: refactor to support sem analysis of subtemplates
                Exact "Not supported"

        let checkFrag (i: int) (f: TemplateFragmentEx) =
            match f with
            | TemplateFragmentEx.Comment s -> None
            | TemplateFragmentEx.Raw "" -> None

            | TemplateFragmentEx.Raw s -> Some (Raw s)

            | TemplateFragmentEx.Discard pat ->
                let checkedPat = checkPatternEx i pat
                match inferDefaultFunc checkedPat with
                | F f -> Discard { Guts = checkedPat; InferDefault = F f } |> Some
                | CannotInfer reason -> 
                    warn i Pattern.discardingWithoutDefault
                        (sprintf "This pattern doesn't take a default value! Discarding it prevents the template from being used to generate text.\n%s" reason)
                    Discard { Guts = checkedPat; InferDefault = CannotInfer reason } |> Some

            | TemplateFragmentEx.Capture (v, pat) ->
                let checkedPat = checkPatternEx i pat
                let required, inferFunc =
                    match inferDefaultFunc checkedPat with
                    | F f -> false, F f
                    | x -> true, x
                let varType = inferPatternType i checkedPat

                if varDefs.ContainsKey v then 
                    warn i Variable.alreadyExists (sprintf "The variable '%s' has already been bound!" v)
                else varDefs.Add(v, { Type = varType; Required = required })

                Capture (v, { Guts = checkedPat; InferDefault = inferFunc }) |> Some

            | TemplateFragmentEx.Define (d, pat) ->
                let checkedPat = checkPatternEx i pat
                let varType = inferPatternType i checkedPat

                if varDefs.ContainsKey d then 
                    warn i PatternDefinition.alreadyExists (sprintf "The pattern macro name '%s' has already been used!" d)
                else patternDefs.Add(d, checkedPat)

                // todo: if pattern macro is object, register object kind in template

                None

        try
            let checkedFrags : TemplateFragment array =
                fragments
                |> Array.mapi (fun i f -> checkFrag i f)
                |> Array.choose id

            Ok (
                { 
                    Body = List.ofArray checkedFrags
                    Variables = Map.ofSeq (varDefs |> Seq.map (|KeyValue|))
                },
                List.ofSeq warnings
            )
        with
        | :? TemplateError as err -> Error err
        | exn -> reraise()