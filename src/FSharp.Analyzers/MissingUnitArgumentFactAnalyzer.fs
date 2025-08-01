module Criipto.FSharp.Analyzers.MissingUnitArgumentFactAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK.ASTCollecting

[<Literal>]
let code = "CRI-XUNIT-002"

[<Literal>]
let name = "Missing unit argument in function with Fact attribute"

[<Literal>]
let msg = "Test functions tagged with the XUnit Fact attribute must have a unit argument or the test runner will not execute them"

let (|AllAttrs|) (attrs: SynAttributes) =
        attrs |> List.collect (fun attrList -> attrList.Attributes)

let (|LongIdentAsString|) (lid: SynLongIdent) =
            lid.LongIdent |> List.map (fun ident -> ident.idText)

let private analyzer
    (_: ISourceText)
    (untypedTree: ParsedInput)
    (_: FSharpCheckFileResults) : Async<Message list> = async {
    let allMissingUnitArgs =
        let allFactFunctionsWithMissingArgs = ResizeArray<string * range>()

        let rec addUntypedPat : SynPat -> unit =
            function
            | SynPat.Paren(inner, _) -> addUntypedPat inner
            | SynPat.Attrib(inner, _, _) -> addUntypedPat inner
            | SynPat.Tuple(_, pats, _, _) -> List.iter addUntypedPat pats
            | SynPat.LongIdent(ident, _, _, pats, _, range) ->
                match pats with
                | SynArgPats.Pats []
                | SynArgPats.NamePatPairs([], _, _) ->
                    let (LongIdentAsString ide) = ident
                    allFactFunctionsWithMissingArgs.Add (String.concat "." ide, range)
                | _ -> ()
            | SynPat.Wild _ -> ()
            | SynPat.Named (SynIdent(ident, _), _, _, range) ->
                allFactFunctionsWithMissingArgs.Add (ident.idText, range)
                ()
            | _ -> ()

        let walker = {
            new SyntaxCollectorBase() with
                override _.WalkBinding(_, SynBinding(_, _, _, _, AllAttrs attrs, _, _, head, _, expr, range, _, _)) =
                    let factAttr =
                        List.exists (fun (attr: SynAttribute) ->
                            let (LongIdentAsString attrName) = attr.TypeName
                            match String.concat "." attrName with
                            | "Fact" -> true
                            | "Xunit.Fact" -> true
                            | _ -> false
                        ) attrs
                    if factAttr then
                        addUntypedPat head
                    ()
        }

        walkAst walker untypedTree
        allFactFunctionsWithMissingArgs |> Seq.toList

    return
        List.map (fun (ident, range) ->
            {
                Type = name
                Message = $"""%s{msg}: %s{ident}"""
                Code = code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        ) allMissingUnitArgs
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
