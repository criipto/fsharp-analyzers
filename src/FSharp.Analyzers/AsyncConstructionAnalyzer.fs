module Idura.FSharp.Analyzers.AsyncConstructionAnalyzer

open System.Collections.Generic

open FSharp.Analyzers.SDK
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns

[<Literal>]
let name = "try or use scoping asynchronous construction"

// Two defects share the shape this analyzer looks for: a 'try' whose value is an asynchronous computation,
// so that the 'try' scopes construction rather than execution.
// They are reported as separate rules:
// - TryWith, where the handler never observes the fault, belongs to 001
// - TryFinally, where cleanup runs while the computation is still going, belongs to 002.

[<Literal>]
let tryConstructionCode = "IDURA-ASYNC-001"

[<Literal>]
let tryConstructionName = "try guarding asynchronous construction"

[<Literal>]
let private tryConstructionMsg =
    "This 'try' guards the construction of the asynchronous computation, not its execution. A fault raised while the computation runs is delivered when it is awaited so the handler never observes it. Await the computation inside the try or handle faults with a combinator such as TaskResult.catch."

[<Literal>]
let private trySequenceConstructionMsg =
    "This 'try' guards the construction of the asynchronous sequence, not its enumeration. Constructing the sequence runs none of the work: a fault is raised when an element is pulled, which happens outside this handler, so the handler never observes it. Enumerate the sequence inside the try, or handle faults where it is consumed."

[<Literal>]
let resourceLifetimeCode = "IDURA-ASYNC-002"

[<Literal>]
let resourceLifetimeName = "resource disposed before the asynchronous computation completes"

// This case is counter-intuitive enough that the generic message gets
// misread as harmless.
[<Literal>]
let private cancellationTokenSourceAddendum =
    " Disposing a CancellationTokenSource does not cancel it. It releases the timer, so a deadline set with CancelAfter or the constructor overload silently never fires, and a library that registers on the token for a retry attempt gets an ObjectDisposedException."

let private cancellationTokenSourceName =
    typeof<System.Threading.CancellationTokenSource>.FullName

let private taskName = typeof<System.Threading.Tasks.Task>.FullName
let private taskOfTName = typedefof<System.Threading.Tasks.Task<obj>>.FullName
let private valueTaskName = typeof<System.Threading.Tasks.ValueTask>.FullName
let private valueTaskOfTName = typedefof<System.Threading.Tasks.ValueTask<obj>>.FullName
let private asyncOfTName = typedefof<Async<obj>>.FullName

let private awaitableTypeNames =
    set [ taskName; taskOfTName; valueTaskName; valueTaskOfTName; asyncOfTName ]

let private asyncEnumerableOfTName =
    typedefof<System.Collections.Generic.IAsyncEnumerable<obj>>.FullName

// AsyncSeq<'T> abbreviates FSharp.Control's own enumerable rather than the framework one. Spelled
// out, because the analyzer does not depend on the package.
[<Literal>]
let private fsharpAsyncEnumerableOfTName = "FSharp.Control.IAsyncEnumerable`1"

let private asyncSequenceTypeNames =
    set [ asyncEnumerableOfTName; fsharpAsyncEnumerableOfTName ]

/// The type an abbreviation stands for, so that wrappers such as
/// TaskResult<_,_> are seen as the types they abbreviate.
let rec private stripAbbreviations (t: FSharpType) =
    if t.IsAbbreviation then stripAbbreviations t.AbbreviatedType else t

/// Whether the given type is an asynchronous sequence.
let private isAsyncSequenceType (t: FSharpType) =
    match TypeNames.basicQualifiedName (stripAbbreviations t) with
    | Some qualifiedName -> asyncSequenceTypeNames.Contains qualifiedName
    | None -> false

/// Whether a value of the given type is a computation that has not finished, or, for a sequence,
/// has not even started, once the expression producing it has been evaluated.
let private isAsyncType (t: FSharpType) =
    match TypeNames.basicQualifiedName (stripAbbreviations t) with
    | Some qualifiedName ->
        awaitableTypeNames.Contains qualifiedName || asyncSequenceTypeNames.Contains qualifiedName
    | None -> false

/// Where the deferred work goes in the rewritten body: on its own, or inside the try the reader has
/// already written.
type private BodyShape =
    | Consume
    | ConsumeInsideTry

/// The computation expression to rewrite the body as, for the type of the flagged expression.
let private computationExpression (shape: BodyShape) (t: FSharpType) =
    let builder, keyword, note =
        match TypeNames.basicQualifiedName (stripAbbreviations t) with
        | Some qualifiedName when qualifiedName = asyncOfTName -> "async", "return!", ""
        | Some qualifiedName when qualifiedName = valueTaskName || qualifiedName = valueTaskOfTName ->
            "task", "return!", " converted back with ValueTask<_>, as FSharp.Core has no ValueTask builder"
        | Some qualifiedName when qualifiedName = fsharpAsyncEnumerableOfTName -> "asyncSeq", "yield!", ""
        | Some qualifiedName when qualifiedName = asyncEnumerableOfTName -> "taskSeq", "yield!", ""
        | _ -> "task", "return!", ""

    let body =
        match shape with
        | Consume -> $"... %s{keyword} ..."
        | ConsumeInsideTry -> $"try ... %s{keyword} ... finally ..."

    $"%s{builder} {{ %s{body} }}%s{note}"

let private cleanupBeforeCompletionMsg (computation: FSharpType) =
    let rewrite = computationExpression ConsumeInsideTry computation

    if isAsyncSequenceType computation then
        $"This 'finally' runs when the asynchronous sequence is constructed and returned, before a single element has been pulled from it, so the cleanup happens before any of the work it guards has run. Move the enumeration inside the 'try' by making the body a computation expression (%s{rewrite})."
    else
        $"This 'finally' runs when the asynchronous computation is constructed and returned, not when it completes, so the cleanup happens while the computation is still running. Move the await inside the 'try' by making the body a computation expression (%s{rewrite})."

/// Whether the resource is a CancellationTokenSource, which the message says more about.
let private isCancellationTokenSource (t: FSharpType) =
    TypeNames.basicQualifiedName t = Some cancellationTokenSourceName

let private mkMessage code ruleName message range = {
    Type = ruleName
    Message = message
    Code = code
    Severity = Severity.Error
    Range = range
    Fixes = []
}

/// The message for a 'use' whose resource the computation still needs.
let private resourceDisposedEarlyMsg (computation: FSharpType) (resource: FSharpMemberOrFunctionOrValue) =
    let typeName = TypeNames.displayName resource.FullType
    let rewrite = computationExpression Consume computation

    let message =
        if isAsyncSequenceType computation then
            $"This 'use' disposes '%s{resource.DisplayName}' when the function returns the asynchronous sequence, which is before a single element has been pulled from it: none of the work that needs the resource has run yet, so every element observes a disposed '%s{typeName}'. Move the enumeration inside the scope by making the body a computation expression (%s{rewrite}), so the 'use' spans enumeration rather than construction."
        else
            $"This 'use' disposes '%s{resource.DisplayName}' when the function returns the asynchronous computation, not when that computation completes. The computation still holds the resource while it runs, so it observes a disposed '%s{typeName}'. Move the await inside the scope by making the body a computation expression (%s{rewrite}), so the 'use' spans execution rather than construction."

    if isCancellationTokenSource resource.FullType then
        message + cancellationTokenSourceAddendum
    else
        message

/// The whole 'use x = acquire ()' binding that declared the value at the given location,
/// or None if that value was not bound by a 'use'.
/// The typed tree lowers a 'use' to the same try/finally one can write by hand,
/// so the untyped tree is what tells the two apart and decides which message to print.
let private useBindingAt (untypedTree: ParsedInput) (declaration: range) =
    let declaredHere (SynBinding(headPat = headPat)) =
        // The declaration location of the bound value sits inside the range of the binding's pattern.
        Range.rangeContainsRange headPat.Range declaration

    let visitor =
        { new SyntaxVisitorBase<range>() with
            override _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.LetOrUse(isUse = true; bindings = bindings; trivia = trivia) ->
                    match List.tryFind declaredHere bindings with
                    | Some binding -> Some(Range.unionRanges trivia.LetOrUseKeyword binding.RangeOfBindingWithRhs)
                    | None -> defaultTraverse expr
                | _ -> defaultTraverse expr }

    SyntaxTraversal.Traverse(declaration.Start, untypedTree, visitor)

/// Whether the 'try'/'with' written at the given range has a handler that catches everything
/// with no 'when' guard.
/// Read from the untyped tree because the typed tree compiles the filter to an ordinary
/// conditional.
let private catchesEverything (untypedTree: ParsedInput) (tryWith: range) =
    let catchesAnything (SynMatchClause(pat = pat; whenExpr = whenExpr)) =
        match whenExpr, pat with
        | None, (SynPat.Wild _ | SynPat.Named _) -> true
        | _ -> false

    let visitor =
        { new SyntaxVisitorBase<bool>() with
            override _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                // An exact range match rather than containment: the position of a nested 'try' is
                // inside the enclosing one as well, and the two can have different handlers.
                | SynExpr.TryWith(withCases = cases) when Range.equals expr.Range tryWith ->
                    Some(List.exists catchesAnything cases)
                | _ -> defaultTraverse expr }

    SyntaxTraversal.Traverse(tryWith.Start, untypedTree, visitor)
    |> Option.defaultValue false

/// Whether any of the given values occurs free anywhere in the expression.
let rec private references (resources: FSharpMemberOrFunctionOrValue list) (e: FSharpExpr) =
    match e with
    | Value v
    | ValueSet(v, _) when resources |> List.exists v.IsEffectivelySameAs -> true
    | _ -> e.ImmediateSubExpressions |> List.exists (references resources)

let private cancellationTokenName =
    typeof<System.Threading.CancellationToken>.FullName

/// Whether a value bound to the given expression is a second handle on the resource that expression
/// referenced, rather than a copy of something read out of it:
///
///     let token = cts.Token                    // a handle: the deadline still lives in the source
///     let content = new StreamContent(stream)  // a wrapper: it reads the stream when it is sent
///     let body = reader.ReadToEnd()            // a copy: the reader is finished with
///
/// Only the cases where retention is certain are listed. Any other call is taken to produce a copy,
/// because that is the reading under which a correctly scoped 'use' stays unreported.
let rec private retains (bound: FSharpMemberOrFunctionOrValue) (rhs: FSharpExpr) =
    match rhs with
    // 'let alias = resource', possibly upcast on the way: the same object under another name.
    | Value _ -> true
    | Coerce(_, inner) -> retains bound inner
    // Anything constructed from the resource holds on to it.
    | NewObject _
    | NewRecord _
    | NewUnionCase _
    | NewTuple _
    | NewArray _ -> true
    // A CancellationToken is a handle on the source that issued it: disposing the source releases
    // the timer the token's deadline depends on.
    | _ -> TypeNames.basicQualifiedName bound.FullType = Some cancellationTokenName

/// Whether the value the expression produces still holds the resource by the time it is returned.
///
/// The walk follows the path evaluation takes to that value: the body of a 'let', the second half of
/// a sequence, both branches of an 'if'. A reference anywhere else has finished with the resource by
/// the time the scope is left, so disposing on return is correctly ordered. The exception is a 'let'
/// binding a second handle rather than a copy, which is the resource under another name.
let rec private tailHolds (resources: FSharpMemberOrFunctionOrValue list) (e: FSharpExpr) =
    match e with
    | Let((bound, rhs, _), body) ->
        let resources =
            if references resources rhs && retains bound rhs then
                bound :: resources
            else
                resources

        tailHolds resources body
    | LetRec(_, body) -> tailHolds resources body
    | Sequential(_, second) -> tailHolds resources second
    | TryFinally(body, _, _, _) -> tailHolds resources body
    | IfThenElse(_, thenExpr, elseExpr) -> tailHolds resources thenExpr || tailHolds resources elseExpr
    | DecisionTree(_, targets) -> targets |> List.exists (snd >> tailHolds resources)
    | _ -> references resources e

/// The static members that hand back a computation which has already finished.
let private isCompletedComputationFactory (m: FSharpMemberOrFunctionOrValue) =
    match m.CompiledName with
    | "FromResult"
    | "FromException"
    | "FromCanceled" ->
        match m.DeclaringEntity |> Option.bind (fun e -> e.TryFullName) with
        | Some owner -> owner = taskName || owner = valueTaskName
        | None -> false
    | _ -> false

/// Whether the asynchronous value the expression produces is one that already existed rather than
/// work the expression starts.
///
/// The walk follows the path evaluation takes to the value, as 'tailHolds' does, so a computation
/// started inside the scope and handed on through a binding reads as one that already existed.
/// This is a false negative, but hard to handle.
let rec private fetchesExistingComputation (e: FSharpExpr) =
    match e with
    | Let(_, body)
    | LetRec(_, body)
    | Sequential(_, body)
    | TryFinally(body, _, _, _) -> fetchesExistingComputation body
    | Coerce(_, inner) -> fetchesExistingComputation inner
    | IfThenElse(_, thenExpr, elseExpr) ->
        fetchesExistingComputation thenExpr && fetchesExistingComputation elseExpr
    | DecisionTree(_, targets) -> targets |> List.forall (snd >> fetchesExistingComputation)
    | Value _
    | FSharpFieldGet _
    | UnionCaseGet _
    | TupleGet _ -> true
    | Call(_, m, _, _, _) ->
        m.IsPropertyGetterMethod
        || Seq.isEmpty m.CurriedParameterGroups
        || isCompletedComputationFactory m
    | NewObject _ -> true
    | _ -> false

/// Whether the guarded body does nothing but produce the asynchronous value.
let rec private guardsOnlyConstruction (e: FSharpExpr) =
    match e with
    | Coerce(_, inner) -> guardsOnlyConstruction inner
    | Let _
    | LetRec _
    | Sequential _
    | IfThenElse _
    | DecisionTree _ -> false
    | _ -> true

let private analyzer
    (_: ISourceText)
    (untypedTree: ParsedInput)
    (typedTree: FSharpImplementationFileContents) : Async<Message list> = async {
    let findings = ResizeArray<Message>()

    // A 'use' lowers to Let(resource, acquire, TryFinally(body, dispose)). The finding is recognised
    // at the Let, where the resource is in scope, but the TryFinally is visited afterwards and must
    // not be reported again. The traversal is pre-order, so the Let comes first.
    let useDerived = HashSet<range>()

    let inspect (e: FSharpExpr) =
        match e with
        // A try/with whose value is an asynchronous computation guards construction rather than
        // execution, so the handler never observes a fault raised while it runs.
        | TryWith(body, _, _, _, _, _, _) when
            isAsyncType e.Type
            && not (fetchesExistingComputation body)
            && (guardsOnlyConstruction body || catchesEverything untypedTree e.Range)
            ->
            let message =
                if isAsyncSequenceType e.Type then
                    trySequenceConstructionMsg
                else
                    tryConstructionMsg

            findings.Add(mkMessage tryConstructionCode tryConstructionName message e.Range)
        // A 'use' whose resource is referenced by the expression that produces the asynchronous
        // value: Dispose runs when that value is returned, so the computation observes a disposed
        // resource while it executes.
        | Let((resource, _, _), (TryFinally(body, _, _, _) as guarded)) when
            isAsyncType guarded.Type && not (fetchesExistingComputation body)
            ->
            match useBindingAt untypedTree resource.DeclarationLocation with
            | None -> ()
            | Some report ->
                useDerived.Add guarded.Range |> ignore

                if tailHolds [ resource ] body then
                    findings.Add(
                        mkMessage
                            resourceLifetimeCode
                            resourceLifetimeName
                            (resourceDisposedEarlyMsg guarded.Type resource)
                            report
                    )
        // A try/finally whose value is an asynchronous computation: the finally
        // block runs when that computation is constructed, so the computation observes a disposed
        // resource while it executes.
        | TryFinally(body, _, _, _) when
            isAsyncType e.Type
            && not (useDerived.Contains e.Range)
            && not (fetchesExistingComputation body)
            ->
            findings.Add(
                mkMessage resourceLifetimeCode resourceLifetimeName (cleanupBeforeCompletionMsg e.Type) e.Range
            )
        | _ -> ()

    let rec visitExpr (e: FSharpExpr) =
        // Looking at an expression forces the compiler to translate it into the typed tree
        // representation exposed here, and that translation is partial: it raises an internal
        // compiler error for constructs it cannot express, a whole member at a time, so one
        // unsupported construct would abort the analysis of every remaining file if we didn't catch
        // it here. The SDK's typed tree walker does the same thing.
        let subExpressions =
            try
                inspect e
                Some e.ImmediateSubExpressions
            with
            | :? System.OperationCanceledException -> reraise ()
            | _ -> None

        match subExpressions with
        | None -> ()
        | Some subExpressions ->
            for sub in subExpressions do
                visitExpr sub

    let rec visitDecl (decl: FSharpImplementationFileDeclaration) =
        match decl with
        | FSharpImplementationFileDeclaration.Entity(_, subDecls) -> List.iter visitDecl subDecls
        // The members the compiler generates contain nothing the user wrote
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, _, _) when v.IsCompilerGenerated -> ()
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(_, _, body) -> visitExpr body
        | FSharpImplementationFileDeclaration.InitAction expr -> visitExpr expr

    List.iter visitDecl typedTree.Declarations

    return List.ofSeq findings
}

[<CliAnalyzer(name)>]
let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    match ctx.TypedTree with
    | None -> async.Return []
    | Some typedTree -> analyzer ctx.SourceText ctx.ParseFileResults.ParseTree typedTree

[<EditorAnalyzer(name)>]
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    match ctx.TypedTree with
    | None -> async.Return []
    | Some typedTree -> analyzer ctx.SourceText ctx.ParseFileResults.ParseTree typedTree
