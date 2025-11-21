module Idura.FSharp.Analyzers.LetWildcardResultAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK.ASTCollecting

[<Literal>]
let code = "IDURA-RESULT-005"

[<Literal>]
let name = "Ignored variable in let! binding."

[<Literal>]
let msg = "The pattern let! _ = ... is dangerous because it makes it easy to accidentally ignore errors."

let private analyzer
    (_: ISourceText)
    (untypedTree: ParsedInput)
    (_: FSharpCheckFileResults) : Async<Message list> = async {
    let allWildLetBangBindings =
        let allWildLetBangBindings = ResizeArray<range>()

        let walker = {
            new SyntaxCollectorBase() with
                override _.WalkExpr(_, expr) =
                    match expr with
                    | SynExpr.LetOrUse(_, false, _, true, [binding], _, _, _) ->
                        match binding with
                        | SynBinding(_, _, _, _, _, _, _, SynPat.Wild _, _, _, range, _, _) ->
                            allWildLetBangBindings.Add range
                        | _ -> ()
                    | _ -> ()
        }

        walkAst walker untypedTree
        allWildLetBangBindings |> Seq.toList

    return
        List.map (fun range ->
            {
                Type = name
                Message = $"""%s{msg}"""
                Code = code
                Severity = Severity.Error
                Range = range
                Fixes = []
            }
        ) allWildLetBangBindings
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
