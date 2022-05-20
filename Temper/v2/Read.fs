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
        | Expr.Var v ->
            match ctx.Variables.TryFind v with
            | None -> failwithf "Variable '%s' not defined" v
            | Some value -> value
        | Expr.Prop (ex, p) -> 
            match eval_expr ex ctx with
            | Val.Obj xs -> 
                match xs.TryFind p with
                | None -> failwithf "Object has no property '%s'" p
                | Some value -> value
            | nonobj -> failwithf "Expected an object but got: %A" nonobj

    let private expr (ex: Expr) : Parser<Val, Context> =
        getUserState >>= fun ctx -> 
            match eval_expr ex ctx with
            | Val.Str s -> pstring s |>> Val.Str
            | v -> preturn v // nyi
    
    let rec private pattern (p: Pattern) (frag_ahead: Frag option) : Parser<Val, Context> =
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
                match ctx.Macros.TryFind id with
                | None -> failwithf "No such pattern '#%s'" id
                | Some pat -> pattern pat frag_ahead
        | Pattern.Macro_With (id, ex) ->
            getUserState >>= fun ctx ->
                match ctx.Macros.TryFind id with
                | None -> failwithf "No such pattern '#%s'" id
                | Some pat -> 
                    match eval_expr ex ctx with
                    | Val.Obj xs -> 
                        (setUserState { ctx with Variables = xs } >>. pattern pat frag_ahead) .>> setUserState ctx
                    | nonobj -> failwithf "Expected an object but got: %A" nonobj
        | Pattern.Subtemplate t ->
            failwith "nyi"
        | Pattern.Expr ex ->
            expr ex
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
                    setUserState { ctx with Variables = ctx.Variables.Add (var, value) }
        | Frag.Define_Macro (id, p) ->
            getUserState >>= fun ctx -> 
                setUserState { ctx with Macros = ctx.Macros.Add (id, p) }
        | Frag.Comment _ | Frag.In_Var _ -> failwith "nyi"

    and private template (t: Template) : Parser<unit, Context> =
        let fs =
            t.Body
            |> Seq.filter (function Frag.Comment _ -> false | _ -> true)
            |> List.ofSeq
        let rec loop fs =
            match fs with
            | [] -> preturn ()
            | x :: y :: fs -> fragment x (Some y) >>. loop (y :: fs)
            | x :: fs -> fragment x None >>. loop fs
        loop fs

    let reader (t: Template) =
        template t .>> eof

    let read (reader: Parser<unit, Context>) (ctx: Context) (str: string) : Result<Context, string> =
        match runParserOnString reader ctx "" str with
        | Success (_, vars, _) -> Result.Ok ctx
        | Failure (msg, _, _) -> Result.Error msg