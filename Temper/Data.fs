namespace OLD.Data

(*
    Data layer - Handling the data that that is being mapped to/from templated text
*)


type VarType =
    | String
    | Option of VarType
    | List of VarType
    | Object of Map<string, VarType>
    override this.ToString() =
        match this with
        | String -> "String"
        | Option ms -> sprintf "%O?" ms
        | List ty -> sprintf "List of %O" ty
        | Object ms -> "{ " + (Map.toSeq ms |> Seq.map (fun (k, vt) -> sprintf "%s: %O" k vt) |> String.concat "; ") + " }"

type ObjectKind = string

type VarValue =
    | String of string
    | Option of VarValue option
    | List of VarValue list
    // Objects store markers of their "kind" - This is the pattern they were parsed with, so it can be used to write them back
    | Object of Map<string, VarValue> * ObjectKind
    override this.ToString() =
        match this with
        | String s -> sprintf "'%s'" s
        | Option opt -> match opt with None -> "null" | Some x -> x.ToString()
        | List ty -> "[ " + (ty |> Seq.map (sprintf "%O") |> String.concat "; ") + " ]"
        | Object (ms, kind) -> kind + " { " + (Map.toSeq ms |> Seq.map (fun (k, v) -> sprintf "%s = %O" k v) |> String.concat "; ") + " }"

type Vars = Map<string, VarValue>

module Var =

    type Type = VarType
    type Value = VarValue
    
    type TypeCheckResult<'T> = Ok of 'T | Error of string * inner: TypeCheckResult<'T> list
    module TypeCheckResult =

        let bind msg = function Ok x -> Ok x | inner -> Error (msg, [inner])
        let fail msg = Error (msg, [])

        let prettyPrint (res: TypeCheckResult<'T>) =
            let mutable s = ""
            let rec f indent res =
                match res with
                | Ok _ -> ()
                | Error (msg, inner) ->
                    s <- s + sprintf "\n%s%s" (String.replicate indent "\t") msg
                    List.iter (f (indent + 1)) inner
            f 0 res
            s

    let rec unifyTypes (a: VarType) (b: VarType) : TypeCheckResult<VarType> =
        match a, b with
        | Type.String, Type.String -> Ok Type.String
        | Type.Option x, Type.Option y -> unifyTypes x y
        | Type.List x, Type.List y -> unifyTypes x y
        | Type.Object xs, Type.Object ys ->
            let mutable newMap = xs
            Seq.choose
                ( fun x ->
                    if xs.ContainsKey x then
                        match unifyTypes xs.[x] ys.[x] |> TypeCheckResult.bind ("Type unification failed for key '" + x + "'") with
                        | Ok unified -> 
                            newMap <- Map.add x unified newMap
                            None
                        | err -> Some err
                    else
                        newMap <- Map.add x ys.[x] newMap
                        None
                ) ys.Keys
            |> List.ofSeq
            |> function [] -> Ok (Type.Object newMap) | xs -> Error ("Error unifying object types", xs)
        | _, _ -> TypeCheckResult.fail (sprintf "Unification not possible, type A is %O and type b is %O" a b)

    let rec checkType (ty: VarType) (value: VarValue) : TypeCheckResult<unit> =
        match ty, value with
        | Type.String, Value.String s -> Ok()
        | Type.Option ty, Value.Option None -> Ok()
        | Type.Option ty, Value.Option (Some x) -> checkType ty x |> TypeCheckResult.bind "Error in option type"
        | Type.List _, Value.List [] -> Ok()
        | Type.List ty, Value.List (x :: xs) -> checkType ty x |> TypeCheckResult.bind "Error in list"
        | Type.Object tms, Value.Object (ms, _) ->
            Seq.choose
                ( fun x ->
                    if ms.ContainsKey x then
                        checkType tms.[x] ms.[x] |> TypeCheckResult.bind ("Type mismatch in key '" + x + "'")
                        |> function Ok _ -> None | err -> Some err
                    else TypeCheckResult.fail ("Missing key '" + x + "'") |> Some
                )
                tms.Keys
            |> List.ofSeq
            |> function [] -> Ok() | xs -> Error ("Error in object", xs)
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
            | Object (ms, kind) ->
                {
                    Get = fun () -> Map.find key ms
                    Set = fun v -> Object (Map.add key v ms, kind) |> this.Set
                }
            | _ -> failwith "Must be an object value"

        // todo: list indexing might get binned due to no sem-time bound checks
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

        member this.IsSome
            with get () =
                match this.Get() with
                | Option (Some x) -> true
                | Option None -> false
                | _ -> failwith "Must be an option value"

        member this.Value
            with get () =
                match this.Get() with
                | Option (Some x) -> x
                | Option None -> failwith "This option value is null"
                | _ -> failwith "Must be an option value"

type VarValue with member this.Lens = Var.Lens.Of this
