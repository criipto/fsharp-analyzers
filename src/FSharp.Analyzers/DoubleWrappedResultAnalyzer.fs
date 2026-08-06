module Idura.FSharp.Analyzers.DoubleWrappedResultAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

[<Literal>]
let code = "IDURA-RESULT-006"

[<Literal>]
let name = "Did you mean to wrap this value in Result twice?"

[<Literal>]
let msg = "Double-wrapping values in Result is often caused by accidentally ignoring an error"

let private analyzer
    (sourceText: ISourceText)
    (_: ParsedInput)
    (typedResults: FSharp.Compiler.Symbols.FSharpImplementationFileContents) : Async<Message list> = async {
    let allDoubleWrappedResultBindings =
        let allBindingsWithDoubleResult = ResizeArray<string * range>()

        let (|LongIdentAsString|) (lid: SynLongIdent) =
            lid.LongIdent |> List.map (fun ident -> ident.idText)

        // If the type is a function, this recurses until it finds the "final" return type of the function
        let rec getReturnType (t: FSharpType) =
            if t.IsFunctionType then
                let rangeType = t.GenericArguments[1]
                getReturnType rangeType
            else
                t

        let RESULT_TYPE_NAME = typedefof<Result<unit,unit>>.FullName

        let isDoubleResult (t: FSharpType) =
            let returnType = getReturnType t

            if TypeNames.basicQualifiedName returnType = Some RESULT_TYPE_NAME then
                let okType = returnType.GenericArguments[0]
                TypeNames.basicQualifiedName okType = Some RESULT_TYPE_NAME
            else
                false

        let walker: TypedTreeCollectorBase = {
            new TypedTreeCollectorBase() with
                override _.WalkLet (var: FSharpMemberOrFunctionOrValue) expr body =
                    match var.FullTypeSafe with
                    | None -> ()
                    | Some t ->
                        if isDoubleResult t then
                            allBindingsWithDoubleResult.Add(var.DisplayName, var.DeclarationLocation)
                override _.WalkMemberOrFunctionOrValue (mfv: FSharpMemberOrFunctionOrValue) _ _ =
                    match mfv.FullTypeSafe with
                    | None -> ()
                    | Some t ->
                        if isDoubleResult t then
                            allBindingsWithDoubleResult.Add(mfv.DisplayName, mfv.DeclarationLocation)
                        
        }

        walkTast walker typedResults
        allBindingsWithDoubleResult |> Seq.toList

    return
        List.map (fun (ident: string, range) ->
            {
                Type = name
                Message = $"""%s{msg}: %s{ident}"""
                Code = code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        ) allDoubleWrappedResultBindings
}

[<CliAnalyzer(name)>]
let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    match ctx.TypedTree with
    | None -> async.Return []
    | Some tast ->
        analyzer ctx.SourceText ctx.ParseFileResults.ParseTree tast

[<EditorAnalyzer(name)>]
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    match ctx.TypedTree with
    | None -> async.Return []
    | Some tast ->
        analyzer ctx.SourceText ctx.ParseFileResults.ParseTree tast