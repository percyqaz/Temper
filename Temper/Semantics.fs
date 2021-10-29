namespace Temper

(*
    Semantic check layer - Verifies the meaning & validity of a template before it can be used.
*)

open System.Collections.Generic
open Temper.Data
open Temper.Tree

module WarningCode =
    module Variable =
        let notFound = "TM001"
        let alreadyExists = "TM002"
    module Pattern =
        let consecutiveAuto = "TM101"
        let discardingWithoutDefault = "TM102"

open WarningCode

type TemplateWarning =
    | Warning of fragPos: int * code: string * message: string
    | Critical of message: string

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

    let check (fragments: TemplateFragmentExt array) : Result<Template * TemplateWarning list, TemplateWarning> =

        let warnings = ResizeArray<TemplateWarning>()
        let warn i code msg = warnings.Add (Warning (i, code, msg))
        let varDefs = Dictionary<string, VarType * bool>()

        let mutable blockAutoFrag = -1
        let rec convertMatchExt (i: int) (m: MatchExt) =
            match m with
            | MatchExt.Variable v ->
                if not (varDefs.ContainsKey v) then
                    warn i Variable.notFound (sprintf "The variable '%s' doesn't exist!" v)
                Match.Variable v
            | MatchExt.Exact s -> Match.Exact s
            | MatchExt.CaseInsensitive s -> Match.CaseInsensitive s
            | MatchExt.Whitespace wd -> Match.Whitespace wd
            | MatchExt.Regex expr -> Match.Regex expr
            | MatchExt.Auto ->
                if blockAutoFrag = i then
                    warn i Pattern.consecutiveAuto "An auto pattern cannot be followed directly by another auto pattern to avoid ambiguous parsing."
                blockAutoFrag <- i + 1
                Match.Auto
            | MatchExt.Choice (head, rest) -> Match.Choice ((convertMatchExt i head), (convertMatchExt i rest))

        let checkFrag (i: int) (f: TemplateFragmentExt) =
            match f with
            | TemplateFragmentExt.Raw s -> Raw s
            | TemplateFragmentExt.Discard m ->
                let cm = convertMatchExt i m
                if not (Match.canInferDefault cm) then warn i Pattern.discardingWithoutDefault "This pattern doesn't take a default value, so you shouldn't discard it!"
                TemplateFragment.Discard cm
            | TemplateFragmentExt.Variable (v, m) ->
                let cm = convertMatchExt i m
                let required = not (Match.canInferDefault cm)
                if varDefs.ContainsKey v then warn i Variable.alreadyExists "This variable has already been used!" else varDefs.Add(v, (VarType.String, required))
                TemplateFragment.Variable (v, cm)

        try
            let checkedFrags : TemplateFragment array = Array.zeroCreate fragments.Length
            Array.iteri (fun i f -> checkedFrags.[i] <- checkFrag i f) fragments

            Result.Ok (
                { 
                    Body = List.ofArray checkedFrags
                    Variables = Map.ofSeq (varDefs |> Seq.map (|KeyValue|))
                    Parameters = Map.empty
                },
                List.ofSeq warnings
            )
        with exn -> Result.Error (Critical exn.Message)