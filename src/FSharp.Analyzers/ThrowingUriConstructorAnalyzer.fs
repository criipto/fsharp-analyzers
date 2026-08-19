module Idura.FSharp.Analyzers.ThrowingUriConstructorAnalyzer

open System.Text.RegularExpressions

open FSharp.Analyzers.SDK
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

[<Literal>]
let code = "IDURA-URI-001"

[<Literal>]
let name = "Throwing Uri or UriBuilder constructor"

[<Literal>]
let msg =
    "Uri and UriBuilder constructors throw on malformed input. Prefer Uri.TryCreate so the failure \
     can be handled as a value."

/// The code to write in a 'fsharpanalyzer: ignore-...' comment to allow a throwing constructor.
///
/// This is deliberately NOT the code of the message the analyzer emits.
///
/// The SDK applies ignore comments itself, in Client.RunAnalyzers, by dropping every message whose
/// Code is named in a directive covering its line. That filter runs after the analyzer has
/// returned and Client.shouldIgnoreMessage is not public, so an analyzer cannot opt out of it. Were
/// the directive to name IDURA-URI-001, it would delete any IDURA-URI-001 message on that line,
/// including the one below, complaining that the directive carries no justification. The rule could
/// then be silenced with no reason given.
///
/// Naming a code we never emit means the SDK matches it against nothing and filters nothing, so the
/// analyzer keeps control and one code can carry both messages. The SDK's comment parser is generic
/// over the code string, so it still parses these directives for us.
/// 
/// This is a bit brittle and annoying to maintain in the analyzer itself, so this issue in the SDK
/// tracks whether there is SDK-level support for this:
/// https://github.com/ionide/FSharp.Analyzers.SDK/issues/308
/// If SDK-level support is added, we can simplify this analyzer significantly and remove this.
[<Literal>]
let ignoreCode = "IDURA-URI-ALLOW-THROW"

/// What a comment block has to open with to count as an explanation. Requiring it keeps a section
/// header or a doc comment that happens to sit above a directive from passing as a reason.
/// The wording asks the author to write why throwing is intended rather than merely noting that it happens.
[<Literal>]
let magicWord = "Should throw because:"

[<Literal>]
let unjustifiedMsg =
    "This throwing constructor is suppressed with " + ignoreCode + ", but no reason is given. Put a \
     comment starting with '" + magicWord + "' above the directive, saying why throwing is what you \
     want here."

let private uriTypeName = typeof<System.Uri>.FullName

let private uriBuilderTypeName = typeof<System.UriBuilder>.FullName

/// A 'fsharpanalyzer:' directive that takes codes. 'ignore-region-end' takes none and so can never
/// name our code; it is matched here only so that it terminates a justification comment block.
let private directivePattern =
    @"fsharpanalyzer:\signore-(line-next|line|region-start|region-end|file)"

/// The codes the directive on this line names, or None when the line has no directive.
/// Internal rather than private so that the tests can exercise it directly.
let internal directiveCodes (line: string) =
    let m = Regex.Match(line, directivePattern + @"\s(.*)$")

    if m.Success then
        m.Groups[2].Value.Split(',')
        |> Array.map (fun s -> s.Trim())
        |> Array.toList
        |> Some
    elif Regex.IsMatch(line, directivePattern) then
        // 'ignore-region-end', or a directive with nothing after it.
        Some []
    else
        None

/// Analyzer codes look like IDURA-URI-001 or IDURA-URI-ALLOW-THROW. Anything else in the
/// comma-separated tail of a directive is prose, which is what we are after.
let private isAnalyzerCode (token: string) = Regex.IsMatch(token, @"^IDURA(-[A-Z0-9]+)+$")

let private lineAt (sourceText: ISourceText) (line: int) =
    // The SDK reports comment lines with 1-based numbering like FCS. ISourceText indexes from 0.
    if line >= 1 && line <= sourceText.GetLineCount() then
        Some(sourceText.GetLineString(line - 1))
    else
        None

/// The bodies of the contiguous run of plain '//' comments immediately above the given line,
/// outermost first. A blank line, a line of code, or another directive ends the run.
let private commentBlockAbove (sourceText: ISourceText) (directiveLine: int) =
    let rec collect line acc =
        match lineAt sourceText line |> Option.map (fun text -> text.Trim()) with
        | Some trimmed when trimmed.StartsWith "//" && (directiveCodes trimmed).IsNone ->
            collect (line - 1) (trimmed.TrimStart '/' :: acc)
        | _ -> acc

    collect (directiveLine - 1) []

/// Whether a comment block opens with the magic word and then says something.
/// The word may be on any line of the block and the explanation itself may start on the line after the word.
let private blockExplains (block: string list) =
    let rec afterMagicWord (lines : string list) =
        match lines with
        | [] -> []
        | line :: rest ->
            let trimmed = line.Trim()

            if trimmed.StartsWith(magicWord, System.StringComparison.OrdinalIgnoreCase) then
                trimmed.Substring magicWord.Length :: rest
            else
                afterMagicWord rest

    afterMagicWord block |> List.exists (fun line -> line.Trim() <> "")

/// Whether the directive on the given line explains itself, either through a comma-separated tail
/// of prose or through a block of plain '//' comments immediately above it.
let private isJustified (sourceText: ISourceText) (directiveLine: int) =
    let hasProseTail =
        match lineAt sourceText directiveLine |> Option.bind directiveCodes with
        | None -> false
        | Some codes -> codes |> List.exists (fun token -> token <> "" && not (isAnalyzerCode token))

    hasProseTail || blockExplains (commentBlockAbove sourceText directiveLine)

let private codeComments (untypedTree: ParsedInput) =
    match untypedTree with
    | ParsedInput.ImplFile implFile -> implFile.Trivia.CodeComments
    | ParsedInput.SigFile sigFile -> sigFile.Trivia.CodeComments

/// The lines holding an 'ignore-file' directive naming our code. AnalyzerIgnoreRange.File does not
/// record where it came from, unlike the other three cases, so it has to be recovered from trivia.
let private ignoreFileDirectiveLines (sourceText: ISourceText) (untypedTree: ParsedInput) =
    codeComments untypedTree
    |> List.choose (fun comment ->
        match comment with
        // The SDK does not support block comments, so neither do we.
        | CommentTrivia.BlockComment _ -> None
        | CommentTrivia.LineComment range ->
            match lineAt sourceText range.StartLine with
            | Some text when Regex.IsMatch(text, @"fsharpanalyzer:\signore-file\s") ->
                match directiveCodes text with
                | Some codes when List.contains ignoreCode codes -> Some range.StartLine
                | _ -> None
            | _ -> None)

/// The lines of the directives that suppress a message at the given range, following the same
/// off-by-one conventions as the SDK's own Client.shouldIgnoreMessage.
let private suppressingDirectiveLines
    (fileDirectiveLines: int list)
    (ignoreRanges: AnalyzerIgnoreRange list)
    (range: range)
    =
    ignoreRanges
    |> List.collect (fun ignoreRange ->
        match ignoreRange with
        | File -> fileDirectiveLines
        | CurrentLine line -> if range.StartLine = line then [ line ] else []
        | NextLine line -> if range.StartLine - 1 = line then [ line ] else []
        | Range(commentStart, commentEnd) ->
            if range.StartLine - 1 >= commentStart && range.EndLine - 1 <= commentEnd then
                [ commentStart ]
            else
                [])

/// Whether this constructor can throw on a malformed argument.
/// Every public Uri constructor can.
/// For UriBuilder, only the parameterless one and the one taking an already-built Uri
/// cannot.
/// Allow-listing those two rather than enumerating the throwing signatures means any
/// overload added in a future framework is reported until we have looked at it.
let private isThrowingConstructor (mfv: FSharpMemberOrFunctionOrValue) =
    if not mfv.IsConstructor then
        false
    else
        match mfv.DeclaringEntity |> Option.bind (fun entity -> entity.TryFullName) with
        | Some typeName when typeName = uriTypeName -> true
        | Some typeName when typeName = uriBuilderTypeName ->
            let parameters = mfv.CurriedParameterGroups |> Seq.collect id |> Seq.toList

            match parameters with
            | [] -> false
            | [ parameter ] -> TypeNames.basicQualifiedName parameter.Type <> Some uriTypeName
            | _ -> true
        | _ -> false

let private analyzer
    (sourceText: ISourceText)
    (untypedTree: ParsedInput)
    (ignoreRanges: Map<string, AnalyzerIgnoreRange list>)
    (symbolUses: FSharpSymbolUse seq)
    : Async<Message list> =
    async {
        let constructions =
            symbolUses
            |> Seq.choose (fun symbolUse ->
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as mfv when isThrowingConstructor mfv -> Some symbolUse.Range
                | _ -> None)
            |> Seq.distinct
            |> Seq.toList

        let ignoreRanges = ignoreRanges |> Map.tryFind ignoreCode |> Option.defaultValue []

        // Only pay for the trivia scan when an ignore-file directive could actually be in play.
        let fileDirectiveLines =
            if ignoreRanges |> List.contains AnalyzerIgnoreRange.File then
                ignoreFileDirectiveLines sourceText untypedTree
            else
                []

        return
            constructions
            |> List.choose (fun range ->
                let directiveLines = suppressingDirectiveLines fileDirectiveLines ignoreRanges range

                match directiveLines with
                | [] -> Some msg
                | lines when lines |> List.exists (isJustified sourceText) -> None
                | _ -> Some unjustifiedMsg
                |> Option.map (fun message -> {
                    Type = name
                    Message = message
                    Code = code
                    Severity = Severity.Error
                    Range = range
                    Fixes = []
                }))
    }

[<CliAnalyzer(name)>]
let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    analyzer ctx.SourceText ctx.ParseFileResults.ParseTree ctx.AnalyzerIgnoreRanges (ctx.GetAllSymbolUsesOfFile())

[<EditorAnalyzer(name)>]
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    match ctx.CheckFileResults with
    | None -> async.Return []
    | Some _ ->
        analyzer ctx.SourceText ctx.ParseFileResults.ParseTree ctx.AnalyzerIgnoreRanges (ctx.GetAllSymbolUsesOfFile())
