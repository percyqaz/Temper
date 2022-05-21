namespace Temper

module Tree =

    type [<RequireQualifiedAccess>] Whitespace =
        | None
        | Tab
        | Space
        | Newline
        member this.AsString =
            match this with
            | None -> ""
            | Tab -> "\t"
            | Space -> " "
            | Newline -> "\n"

    type [<RequireQualifiedAccess>] Expr =
        | Var of string
        | Prop of Expr * prop: string
        | It

    type [<RequireQualifiedAccess>] Pattern =
        | Exact of string
        | CaseInsensitive of string
        | Regex of string
        | Whitespace of Whitespace
        | String_Expr of Expr
        | List_Expr of Expr * Pattern
        | Obj_Expr of Expr * Pattern
        | Choice of Pattern * Pattern
        | Star of Pattern
        | Macro of string
        | Subtemplate of Template
        | WithDefault of Pattern * string
        | Auto

    and [<RequireQualifiedAccess>] Val =
        | Nil
        | Str of string
        | List of Val list
        | Obj of Map<string, Val>

    and [<RequireQualifiedAccess>] Frag =
        | Pure of string
        | Comment of string
        | Discard of Pattern
        | Capture of ident: string * Pattern
        //| In_Var of ident: string
        | Define_Macro of ident: string * Pattern

    and Template =
        {
            Body: Frag list
        }

    type Context =
        {
            Variables: Map<string, Val * int>
            Macros: Map<string, Pattern>
            Depth: int
        }
        static member Empty = { Variables = Map.empty; Macros = Map.empty; Depth = 0 }

        member this.Macro (id: string) =
            match this.Macros.TryFind id with
            | None -> failwithf "Macro '#%s' not defined" id
            | Some value -> value
        member this.WithMacro (id: string, pat: Pattern) =
            { this with Macros = Map.add id pat this.Macros }

        member this.Var (v: string) =
            match this.Variables.TryFind v with
            | None -> failwithf "Variable '%s' not defined" v
            | Some value -> fst value
        member this.WithVar (v: string, value: Val) =
            { this with Variables = Map.add v (value, this.Depth) this.Variables }

        member this.NewScope =
            { this with Depth = this.Depth + 1 }
        member this.EndScope =
            this.Variables |> Map.filter (fun _ (_, d) -> d >= this.Depth)
            |> Map.map (fun _ -> fst)
            |> Val.Obj
        member this.WithScope (xs: Map<string, Val>) =
            let vars = Map.fold (fun ys k v -> Map.add k (v, this.Depth + 1) ys) this.Variables xs
            { this with Variables = vars; Depth = this.Depth + 1 }