namespace Temper

open Temper.Tree
open FParsec

module Read =

    let private pstring_allow_newline (str: string) =
        let parts = str.Split "\n" |> List.ofArray
        let rec f xs : Parser<unit, _> =
            match xs with
            | x :: y :: xs -> skipString x .>> pchar '\n' .>> f (y :: xs)
            | x :: [] -> skipString x
            | [] -> failwith "impossible"
        f parts >>% str

    let rec private eval_expr (ex: Expr) (ctx: Context) =
        match ex with
        | Expr.It -> ctx.Var "_"
        | Expr.Var v -> ctx.Var v
        | Expr.Prop (ex, p) -> 
            match eval_expr ex ctx with
            | Val.Obj xs -> 
                match xs.TryFind p with
                | None -> failwithf "Object has no property '%s'" p
                | Some value -> value
            | nonobj -> failwithf "Expected an object but got: %A" nonobj

    let private string_expr (ex: Expr) : Parser<Val, Context> =
        getUserState >>= fun ctx -> 
            match eval_expr ex ctx with
            | Val.Str s -> pstring s |>> Val.Str
            | Val.Nil -> preturn Val.Nil
            | Val.List _ -> failwith "Cannot directly parse text against a list"
            | Val.Obj _ -> failwith "Cannot directly parse text against an object"

    let rec private list_expr (ex: Expr) (pat: Pattern) (frag_ahead: Frag option) : Parser<Val, Context> =
        getUserState >>= fun ctx -> 
            match eval_expr ex ctx with
            | Val.List xs ->
                let rec loop xs : Parser<Val list, Context> =
                    match xs with
                    | [] -> preturn []
                    | x :: xs ->
                        getUserState >>= fun (ctx: Context) ->
                            setUserState (ctx.WithVar("_", x).NewScope)
                            >>. pattern pat frag_ahead
                            .>> setUserState ctx
                        >>= fun x -> loop xs |>> fun xs -> x :: xs
                loop xs |>> Val.List
            | other -> failwithf "Expected a list but got: %A" other

    and private obj_expr (ex: Expr) (pat: Pattern) (frag_ahead: Frag option) : Parser<Val, Context> =
        getUserState >>= fun ctx -> 
            match eval_expr ex ctx with
            | Val.Obj xs ->
                getUserState >>= fun (ctx: Context) -> setUserState (ctx.WithScope xs)
                    >>. pattern pat frag_ahead
                    .>> setUserState ctx
            | other -> failwithf "Expected an object but got: %A" other
    
    and private pattern (p: Pattern) (frag_ahead: Frag option) : Parser<Val, Context> =
        match p with
        | Pattern.Exact s -> pstring s |>> Val.Str
        | Pattern.CaseInsensitive s -> pstringCI s |>> Val.Str
        | Pattern.Whitespace _ -> skipped spaces |>> Val.Str
        | Pattern.Choice (head, rest) -> pattern head frag_ahead <|> pattern rest frag_ahead
        | Pattern.Regex ex -> regex ex |>> Val.Str
        | Pattern.Star pat -> many (pattern pat frag_ahead) |>> Val.List
        | Pattern.WithDefault (pat, def) -> pattern pat frag_ahead
        | Pattern.Macro id ->
            getUserState >>= fun ctx ->
                pattern (ctx.Macro id) frag_ahead
        | Pattern.Subtemplate t ->
            getUserState >>= fun ctx ->
                setUserState ctx.NewScope
                >>. template t frag_ahead >>. getUserState >>= fun new_ctx -> 
                    setUserState ctx >>. preturn new_ctx.EndScope
        | Pattern.String_Expr ex ->
            string_expr ex
        | Pattern.List_Expr (ex, pat) ->
            list_expr ex pat frag_ahead
        | Pattern.Obj_Expr (ex, pat) ->
            obj_expr ex pat frag_ahead
        | Pattern.Auto ->
            match frag_ahead with
            | Some f -> (manyCharsTill anyChar (followedBy (fragment f None)))
            | None -> manyCharsTill anyChar eof
            |>> Val.Str

    and private fragment (frag: Frag) (frag_ahead: Frag option) : Parser<unit, Context> =
        match frag with
        | Frag.Pure s -> pstring_allow_newline s >>% ()
        | Frag.Discard p -> pattern p frag_ahead >>% ()
        | Frag.Capture (var, p) -> 
            pattern p frag_ahead >>= fun value -> 
                getUserState >>= fun ctx -> 
                    setUserState (ctx.WithVar(var, value))
        | Frag.Define_Macro (id, p) ->
            getUserState >>= fun ctx -> 
                setUserState { ctx with Macros = ctx.Macros.Add (id, p) }
        | Frag.Comment _ -> failwith "impossible, these have been filtered out"

    and private template (t: Template) (frag_ahead: Frag option) : Parser<unit, Context> =
        let fs =
            t.Body
            |> Seq.filter (function Frag.Comment _ -> false | _ -> true)
            |> List.ofSeq
        let rec loop fs =
            match fs with
            | [] -> preturn ()
            | x :: y :: fs -> fragment x (Some y) >>. loop (y :: fs)
            | x :: fs -> fragment x frag_ahead >>. loop fs
        loop fs

    let reader (t: Template) =
        template t None .>> eof

    let read (reader: Parser<unit, Context>) (ctx: Context) (str: string) : Result<Context, string> =
        match runParserOnString reader ctx "" str with
        | Success (_, vars, _) -> Result.Ok ctx
        | Failure (msg, _, _) -> Result.Error msg