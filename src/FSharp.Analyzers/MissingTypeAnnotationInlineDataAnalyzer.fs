module Criipto.FSharp.Analyzers.MissingTypeAnnotationInlineDataAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis

[<Literal>]
let code = "CRI-XUNIT-001"

[<Literal>]
let name = "Missing type annotations in XUnit InlineData argument"

[<Literal>]
let msg = "Arguments with data supplied via the InlineData annotation must have type annotations"

let private analyzer
    (_: ISourceText)
    (untypedTree: ParsedInput)
    (_: FSharpCheckFileResults) : Async<Message list> = async {
    let allInlineDataTheories =
        let allUntypedArgsWithInlineData = ResizeArray<(Ident option) * range>()

        let (|LongIdentAsString|) (lid: SynLongIdent) =
            lid.LongIdent |> List.map (fun ident -> ident.idText)

        let rec addUntypedPat : SynPat -> unit =
            function
            | SynPat.Paren(inner, _) -> addUntypedPat inner
            | SynPat.Attrib(inner, _, _) -> addUntypedPat inner
            | SynPat.Tuple(_, pats, _, _) -> List.iter addUntypedPat pats
            | SynPat.LongIdent(_, _, _, pats, _, _) -> addUntypedArgPats pats
            | SynPat.Wild range ->
                allUntypedArgsWithInlineData.Add (None, range)
            | SynPat.Named (SynIdent(id, _), _, _, _) ->
                allUntypedArgsWithInlineData.Add (Some id, id.idRange)
            | _ -> ()
        and addUntypedNamePatPair (_, _, pat) : unit =
            addUntypedPat pat
        and addUntypedArgPats : SynArgPats -> unit =
            function
            | SynArgPats.NamePatPairs(pats, _, _) -> List.iter addUntypedNamePatPair pats
            | SynArgPats.Pats pats -> List.iter addUntypedPat pats

        let walker = {
            new SyntaxCollectorBase() with
                override _.WalkBinding(path, binding) =
                    match binding with
                    | SynBinding(_, SynBindingKind.Normal, _, _, attrs, _, valData, pat, _, _, range, _, _) ->
                        List.tryPick
                            (fun (attr: SynAttribute) ->
                                let (LongIdentAsString value) = attr.TypeName
                                match value with
                                | ["InlineDataAttribute"]
                                | ["InlineData"]
                                | ["Xunit"; "InlineDataAttribute"]
                                | ["Xunit"; "InlineData"] ->
                                    addUntypedPat pat |> Some 
                                | _ -> None
                            )
                            (attrs |> List.collect (fun attrList -> attrList.Attributes))
                       |> ignore
                    | _ -> ()
        }

        walkAst walker untypedTree
        allUntypedArgsWithInlineData |> Seq.toList

    return
        List.map (fun (ident: Ident option, range) ->
            {
                Type = name
                Message = $"""%s{msg}: %s{match ident with None -> "" | Some id -> id.idText}"""
                Code = code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        ) allInlineDataTheories
}

[<CliAnalyzer(name)>]
let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    analyzer ctx.SourceText ctx.ParseFileResults.ParseTree ctx.CheckFileResults

[<EditorAnalyzer(name)>]
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    match ctx.CheckFileResults with
    | None -> async.Return []
    | Some checkResults ->
        analyzer ctx.SourceText ctx.ParseFileResults.ParseTree checkResults