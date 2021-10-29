namespace Temper.Data

(*
    Data layer - Handling the data that that is being mapped to/from templated text
*)

type VarType =
    | String
    | List of VarType
    | Object of Map<string, VarType>
    override this.ToString() =
        match this with
        | String -> "String"
        | List ty -> sprintf "List of %O" ty
        | Object ms -> "{ " + (Map.toSeq ms |> Seq.map (fun (k, vt) -> sprintf "%s: %O" k vt) |> String.concat "; ") + " }"

type VarValue =
    | String of string
    | List of VarValue list
    | Object of Map<string, VarValue>
    override this.ToString() =
        match this with
        | String s -> sprintf "'%s'" s
        | List ty -> "[ " + (ty |> Seq.map (sprintf "%O") |> String.concat "; ") + " ]"
        | Object ms -> "{ " + (Map.toSeq ms |> Seq.map (fun (k, v) -> sprintf "%s = %O" k v) |> String.concat "; ") + " }"

type Vars = Map<string, VarValue>

module Var =

    type Type = VarType
    type Value = VarValue
    
    type TypeCheckResult = Ok | Error of string * inner: TypeCheckResult list
    module TypeCheckResult =

        let bind msg = function Ok -> Ok | inner -> Error (msg, [inner])
        let fail msg = Error (msg, [])

        let prettyPrint (res: TypeCheckResult) =
            let mutable s = ""
            let rec f indent res =
                match res with
                | Ok -> ()
                | Error (msg, inner) ->
                    s <- s + sprintf "\n%s%s" (String.replicate indent "\t") msg
                    List.iter (f (indent + 1)) inner
            f 0 res
            s

    let rec checkType (ty: VarType) (value: VarValue) : TypeCheckResult =
        match ty, value with
        | Type.String, Value.String s -> Ok
        | Type.List _, Value.List [] -> Ok
        | Type.List ty, Value.List (x :: xs) -> checkType ty x |> TypeCheckResult.bind "Error in list"
        | Type.Object tms, Value.Object ms ->
            Seq.choose
                ( fun x ->
                    if ms.ContainsKey x then
                        checkType tms.[x] ms.[x] |> TypeCheckResult.bind ("Type mismatch in key '" + x + "'")
                        |> function Ok -> None | err -> Some err
                    else TypeCheckResult.fail ("Missing key '" + x + "'") |> Some
                )
                tms.Keys
            |> List.ofSeq
            |> function [] -> Ok | xs -> Error ("Error in object", xs)
        | _, _ -> TypeCheckResult.fail (sprintf "Expected %O but got %O" ty value)

    type Lens =
        {
            Get: unit -> Value
            Set: Value -> Value
        }
        static member Of (value: Value) = { Get = (fun () -> value); Set = id }
        member this.Item (key: string) =
            let parent = this.Get()
            match parent with
            | Object ms ->
                {
                    Get = fun () -> Map.find key ms
                    Set = fun v -> Map.add key v ms |> Object |> this.Set
                }
            | _ -> failwith "Must be an object value"
        member this.Item (i: int) =
            let parent = this.Get()
            match parent with
            | List xs ->
                {
                    Get = fun () -> xs.[i]
                    Set = fun v ->
                        if i = xs.Length then xs @ [v] |> List |> this.Set
                        else List.mapi (fun n x -> if i = n then v else x) xs |> List |> this.Set
                }
            | _ -> failwith "Must be a list value"
        member this.Text
            with get () =
                match this.Get() with
                | String s -> s
                | _ -> failwith "Must be a string value"

type VarValue with member this.Lens = Var.Lens.Of this
