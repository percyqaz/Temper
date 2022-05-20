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

    type [<RequireQualifiedAccess>] Pattern =
        | Exact of string
        | CaseInsensitive of string
        | Regex of string
        | Whitespace of Whitespace
        | Expr of Expr
        | Choice of Pattern * Pattern
        | Star of Pattern
        | Macro of string
        | Macro_With of string * Expr
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
        | In_Var of ident: string
        | Define_Macro of ident: string * Pattern

    and Template =
        {
            Body: Frag list
        }

    type Context =
        {
            Variables: Map<string, Val>
            Macros: Map<string, Pattern>
        }