module Idura.FSharp.Analyzers.DoResultIgnoreAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK.ASTCollecting

let codePattern code = $"IDURA-DO-IGNORE-{code}"

[<Literal>]
let name = "Do not ignore results in do! statements"

let msgPattern func = $"The pattern do! ... |> {func} is dangerous because it makes it easy to accidentally ignore errors."

let (|SynLongIdentAsString|) (lid: SynLongIdent) =
    lid.LongIdent |> List.map (fun ident -> ident.idText)

let (|LongIdentAsString|Other|) (lid: SynExpr) =
    match lid with
    | SynExpr.LongIdent(_, id, _, _) ->
        let (SynLongIdentAsString ident) = id
        LongIdentAsString ident
    | _ -> Other

let private analyzer
    (_: ISourceText)
    (untypedTree: ParsedInput)
    (_: FSharpCheckFileResults) : Async<Message list> = async {
    let allIgnoredResultsInDo =
        let allDoBindingsWithReturnIgnorePipe = ResizeArray<range * string * string>()

        // Patterns to find:
        // _ |> Result.ignore
        // _ |> TaskResult.ignore
        // _ |> Result.map ignore
        // _ |> TaskResult.map ignore
        //
        // There  is no need to find the Option versions, since they do require the inner
        // success type to be an option and are thus not susceptible to accidental
        // double-wrapping of results.

        let rec addResultIgnore range  = function
        | SynExpr.App(_, _, funcExpr: SynExpr, argExpr, _) ->
            match funcExpr with
            // op_PipeRight is the internal name for the pipe right |> operator.
            // Internal operation names are always mapped to long identifiers.
            | SynExpr.App(_, _, LongIdentAsString ["op_PipeRight"], _, _) ->
                // The FsToolkit modules are tagged with RequireqQualifiedAccess, so
                // we only need to look for long identifiers.
                match argExpr with
                | LongIdentAsString ["Result"; "ignore"] ->
                    allDoBindingsWithReturnIgnorePipe.Add (range, "Result.ignore", "001")
                | LongIdentAsString ["TaskResult"; "ignore"] ->
                    allDoBindingsWithReturnIgnorePipe.Add (range, "TaskResult.ignore", "002")
                | SynExpr.App(_, _, func, SynExpr.Ident id, _) when id.idText = "ignore" ->
                    match func with
                    | LongIdentAsString ["Result"; "map"] ->
                        allDoBindingsWithReturnIgnorePipe.Add (range, "Result.map ignore", "003")
                    | LongIdentAsString ["TaskResult"; "map"] ->
                        allDoBindingsWithReturnIgnorePipe.Add (range, "TaskResult.map ignore", "004")
                    | _ -> ()
                | _ -> ()
            | _ -> ()
        | SynExpr.DebugPoint(_, _, expr) -> addResultIgnore range expr
        | SynExpr.DiscardAfterMissingQualificationAfterDot(expr, _, _) -> addResultIgnore range expr
        | SynExpr.Downcast(expr, _, _) -> addResultIgnore range expr
        | SynExpr.For(_, _, _, _, _, _, _, expr, _) -> addResultIgnore range expr
        | SynExpr.ForEach(_, _, _, _, _, _, expr, _) -> addResultIgnore range expr
        | SynExpr.FromParseError(expr, _) -> addResultIgnore range expr
        | SynExpr.IfThenElse(_, expr1, expr2, _, _, _, _) ->
            addResultIgnore range expr1; Option.iter (addResultIgnore range) expr2
        | SynExpr.InferredDowncast(expr, _) -> addResultIgnore range expr
        | SynExpr.InferredUpcast(expr, _) -> addResultIgnore range expr
        | SynExpr.JoinIn(expr1, _, expr2, _) ->
            addResultIgnore range expr1
            addResultIgnore range expr2
        | SynExpr.Lazy(expr, _) -> addResultIgnore range expr
        | SynExpr.Match(_, _, clauses, _, _) ->
            List.iter (fun clause ->
                match clause with
                | SynMatchClause(_, _, expr, _, _, _) ->
                    addResultIgnore range expr
            ) clauses
        | SynExpr.Paren(expr, _, _, _) -> addResultIgnore range expr
        | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
            addResultIgnore range expr1; addResultIgnore range expr2
        | SynExpr.TryFinally(expr1, expr2, _, _, _, _) ->
            addResultIgnore range expr1; addResultIgnore range expr2
        | SynExpr.TryWith(expr1, _, _, _, _, _) ->
            addResultIgnore range expr1
        |  _ -> ()

        let walker = {
            new SyntaxCollectorBase() with
                override _.WalkExpr(_, expr) =
                    match expr with
                    | SynExpr.DoBang(expr, range, _) ->
                        addResultIgnore range expr
                    | _ -> ()
        }

        walkAst walker untypedTree
        allDoBindingsWithReturnIgnorePipe |> Seq.toList

    return
        List.map (fun (range, func, code) ->
            {
                Type = name
                Message = $"""%s{msgPattern func}"""
                Code = codePattern code
                Severity = Severity.Error
                Range = range
                Fixes = []
            }
        ) allIgnoredResultsInDo
}

[<CliAnalyzer(name)>]
let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    analyzer ctx.SourceText ctx.ParseFileResults.ParseTree ctx.CheckFileResults

[<EditorAnalyzer(name)>]
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    match ctx.CheckFileResults with
    | None -> async.Return []
    | Some (checkResults: FSharpCheckFileResults) ->
        analyzer ctx.SourceText ctx.ParseFileResults.ParseTree checkResults
