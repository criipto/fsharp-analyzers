module Idura.FSharp.Analyzers.DoNotUseYourOwnRandomAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK.ASTCollecting

type RngType =
| RNGCryptoServiceProvider
| RandomNumberGenerator

[<Literal>]
let RNGCryptoServiceProviderCode = "IDURA-CRYPTO-001"

[<Literal>]
let RandomNumberGeneratorCode = "IDURA-CRYPTO-002"

[<Literal>]
let name = "Do not use your own random generator instance"

[<Literal>]
let RNGCryptoServiceProviderMsg = "Do not use your own instance of RNGCryptoServiceProvider. Depend on a global RNG pool to ensure stability of the generator."

[<Literal>]
let RandomNumberGeneratorMsg = "Do not use your own instance of RandomNumberGenerator. Depend on a global RNG pool to ensure stability of the generator."

let (|SynLongIdentAsString|) (lid: SynLongIdent) =
            lid.LongIdent |> List.map (fun ident -> ident.idText)

let (|LongIdentAsString|Other|) (lid: SynExpr) =
    match lid with
    | SynExpr.LongIdent(_, id: SynLongIdent, _, _) ->
        let (SynLongIdentAsString ident) = id
        LongIdentAsString ident
    | _ -> Other

let private analyzer
    (_: ISourceText)
    (untypedTree: ParsedInput)
    (_: FSharpCheckFileResults) : Async<Message list> = async {
    let allRngInstances =
        let allRngInstances = ResizeArray<RngType * range>()

        let walker = {
            new SyntaxCollectorBase() with
                override _.WalkExpr(_, expr) =
                    match expr with
                    | SynExpr.Ident id when id.idText = "RNGCryptoServiceProvider" ->
                        allRngInstances.Add (RNGCryptoServiceProvider, expr.Range)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["RNGCryptoServiceProvider"]), _, _)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["Cryptography"; "RNGCryptoServiceProvider"]), _, _)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["Security"; "Cryptography"; "RNGCryptoServiceProvider"]), _, _)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["System"; "Security"; "Cryptography"; "RNGCryptoServiceProvider"]), _, _)
                    | LongIdentAsString ["RNGCryptoServiceProvider"]
                    | LongIdentAsString ["Cryptography"; "RNGCryptoServiceProvider"]
                    | LongIdentAsString ["Security"; "Cryptography"; "RNGCryptoServiceProvider"]
                    | LongIdentAsString ["System"; "Security"; "Cryptography"; "RNGCryptoServiceProvider"]
                    | LongIdentAsString ["RNGCryptoServiceProvider"; "Create"]
                    | LongIdentAsString ["Cryptography"; "RNGCryptoServiceProvider"; "Create"]
                    | LongIdentAsString ["Security"; "Cryptography"; "RNGCryptoServiceProvider"; "Create"]
                    | LongIdentAsString ["System"; "Security"; "Cryptography"; "RNGCryptoServiceProvider"; "Create"] ->
                        allRngInstances.Add (RNGCryptoServiceProvider, expr.Range)
                    | SynExpr.Ident id when id.idText = "RandomNumberGenerator" ->
                        allRngInstances.Add (RandomNumberGenerator, expr.Range)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["RandomNumberGenerator"]), _, _)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["Cryptography"; "RandomNumberGenerator"]), _, _)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["Security"; "Cryptography"; "RandomNumberGenerator"]), _, _)
                    | SynExpr.New(_, SynType.LongIdent (SynLongIdentAsString ["System"; "Security"; "Cryptography"; "RandomNumberGenerator"]), _, _)
                    | LongIdentAsString ["RandomNumberGenerator"]
                    | LongIdentAsString ["Cryptography"; "RandomNumberGenerator"]
                    | LongIdentAsString ["Security"; "Cryptography"; "RandomNumberGenerator"]
                    | LongIdentAsString ["System"; "Security"; "Cryptography"; "RandomNumberGenerator"]
                    | LongIdentAsString ["RandomNumberGenerator"; "Create"]
                    | LongIdentAsString ["Cryptography"; "RandomNumberGenerator"; "Create"]
                    | LongIdentAsString ["Security"; "Cryptography"; "RandomNumberGenerator"; "Create"]
                    | LongIdentAsString ["System"; "Security"; "Cryptography"; "RandomNumberGenerator"; "Create"] ->
                        allRngInstances.Add (RandomNumberGenerator, expr.Range)
                    | LongIdentAsString _
                    | _ -> ()
        }

        walkAst walker untypedTree
        allRngInstances |> Seq.toList

    return
        List.map (fun (typ, range) ->
            let msg, code =
                match typ with
                | RNGCryptoServiceProvider -> RNGCryptoServiceProviderMsg, RNGCryptoServiceProviderCode
                | RandomNumberGenerator -> RandomNumberGeneratorMsg, RandomNumberGeneratorCode
            {
                Type = name
                Message = $"""%s{msg}"""
                Code = code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        ) allRngInstances
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
