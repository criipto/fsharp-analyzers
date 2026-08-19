module Idura.FSharp.Analyzers.Tests.ThrowingUriConstructorAnalyzerTests

open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open FSharp.Analyzers.SDK
open Idura.FSharp.Analyzers.ThrowingUriConstructorAnalyzer

let setupContext () = projectOptions "net10.0" []

let private messages file = messagesFor setupContext cliAnalyzer file

let private singleMessageFor file = async {
    let! msgs = messages file
    return Assert.Single msgs
}

[<Theory>]
[<MemberData(nameof (TestFiles.GetSources),
             parameters = [| "throwingUriConstructor/positive" |],
             MemberType = typeof<TestFiles>)>]
let ``positive`` (program: string, filename: string) =
    let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
    runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof (TestFiles.GetSources),
             parameters = [| "throwingUriConstructor/negative" |],
             MemberType = typeof<TestFiles>)>]
let ``negative`` (program: string, _: string) =
    runNegativeTest setupContext cliAnalyzer program

// ---------------------------------------------------------------------------------------------
// The constructors themselves
// ---------------------------------------------------------------------------------------------

[<Theory>]
[<InlineData("UriFromString.fs")>]
[<InlineData("UriWithUriKind.fs")>]
[<InlineData("UriWithCreationOptions.fs")>]
[<InlineData("UriRelative.fs")>]
[<InlineData("UriFromUris.fs")>]
[<InlineData("UriObsoleteDontEscape.fs")>]
[<InlineData("NewKeyword.fs")>]
[<InlineData("FullyQualified.fs")>]
[<InlineData("Aliased.fs")>]
[<InlineData("Pipelined.fs")>]
[<InlineData("InsideTryWith.fs")>]
[<InlineData("UriBuilderFromString.fs")>]
[<InlineData("UriBuilderSchemeHost.fs")>]
[<InlineData("UriBuilderSchemeHostPort.fs")>]
[<InlineData("UriBuilderFull.fs")>]
let ``a throwing constructor is reported once, as IDURA-URI-001`` (file: string) = async {
    let! message = singleMessageFor $"throwingUriConstructor/positive/%s{file}"
    Assert.Equal(code, message.Code)
    Assert.Equal(name, message.Type)
    Assert.Equal(Severity.Error, message.Severity)
    Assert.Equal(msg, message.Message)
}

[<Fact>]
let ``every throwing constructor in a file is reported`` () = async {
    let! msgs = messages "throwingUriConstructor/positive/Multiple.fs"
    Assert.Equal(3, msgs.Length)
}

[<Fact>]
let ``an ignore region does not reach past its end`` () = async {
    let! message = singleMessageFor "throwingUriConstructor/positive/RegionEndsBeforeUse.fs"
    Assert.Equal(msg, message.Message)
    Assert.Equal(10, message.Range.StartLine)
}

// ---------------------------------------------------------------------------------------------
// Disabling without an explanation
// ---------------------------------------------------------------------------------------------

[<Theory>]
[<InlineData("IgnoreLineNoReason.fs")>]
[<InlineData("IgnoreLineNextNoReason.fs")>]
[<InlineData("IgnoreFileNoReason.fs")>]
[<InlineData("WhitespaceTail.fs")>]
[<InlineData("ReasonAboveBlankLine.fs")>]
[<InlineData("DirectiveAboveDirective.fs")>]
[<InlineData("BlockCommentReason.fs")>]
[<InlineData("PlainCommentAbove.fs")>]
[<InlineData("MagicWordWithoutReason.fs")>]
[<InlineData("MagicWordMidSentence.fs")>]
let ``suppressing without a reason reports with a special message`` (file: string) = async {
    let! message = singleMessageFor $"throwingUriConstructor/positive/%s{file}"
    Assert.Equal(code, message.Code)
    Assert.Equal(unjustifiedMsg, message.Message)
}

[<Fact>]
let ``an unexplained ignore region asks for a reason once per throwing constructor`` () = async {
    let! msgs = messages "throwingUriConstructor/positive/IgnoreRegionNoReason.fs"
    Assert.Equal(2, msgs.Length)
    Assert.All(msgs, fun m -> Assert.Equal(unjustifiedMsg, m.Message))
}

[<Theory>]
[<InlineData("WrongCasePrefix.fs")>]
[<InlineData("CodeInsteadOfMarker.fs")>]
let ``a comment that is not a directive for this analyzer suppresses nothing`` (file: string) = async {
    let! message = singleMessageFor $"throwingUriConstructor/positive/%s{file}"
    Assert.Equal(msg, message.Message)
}

// ---------------------------------------------------------------------------------------------
// Disabling with an explanation
// ---------------------------------------------------------------------------------------------

[<Theory>]
[<InlineData("IgnoreLineCommaTail.fs")>]
[<InlineData("IgnoreLineWithBlockAbove.fs")>]
[<InlineData("IgnoreLineNextSingleLineReason.fs")>]
[<InlineData("IgnoreLineNextMultiLineReason.fs")>]
[<InlineData("IgnoreRegionWithReason.fs")>]
[<InlineData("IgnoreFileWithReason.fs")>]
[<InlineData("MultipleCodesWithReason.fs")>]
[<InlineData("MagicWordAfterHeader.fs")>]
[<InlineData("MagicWordReasonBelow.fs")>]
[<InlineData("MagicWordLowerCase.fs")>]
[<InlineData("MagicWordDocComment.fs")>]
let ``an explained suppression silences the analyzer`` (file: string) = async {
    let! msgs = messages $"throwingUriConstructor/negative/%s{file}"
    Assert.Empty msgs
}

// ---------------------------------------------------------------------------------------------
// Reading the codes off a directive line
// ---------------------------------------------------------------------------------------------

[<Theory>]
[<InlineData("let uri = Uri \"http://example.com\"")>]
[<InlineData("// just a comment")>]
// ignore-everything is not a real ignore directive
[<InlineData("// fsharpanalyzer: ignore-everything IDURA-URI-ALLOW-THROW")>]
// The directive marker is matched case-sensitively, as the SDK matches it.
[<InlineData("// FSharpAnalyzer: ignore-line IDURA-URI-ALLOW-THROW")>]
// A comment naming the code but no directive is not a directive.
[<InlineData("// IDURA-URI-ALLOW-THROW")>]
let ``a line with no directive names no codes`` (line: string) =
    Assert.Equal<string list option>(None, directiveCodes line)

[<Theory>]
// Every form that takes codes, so a rename of one of them cannot go unnoticed.
[<InlineData("// fsharpanalyzer: ignore-line IDURA-URI-ALLOW-THROW")>]
[<InlineData("// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW")>]
[<InlineData("// fsharpanalyzer: ignore-region-start IDURA-URI-ALLOW-THROW")>]
[<InlineData("// fsharpanalyzer: ignore-file IDURA-URI-ALLOW-THROW")>]
// The directive comment doesn't need to be on it's own line
[<InlineData("let uri = Uri x // fsharpanalyzer: ignore-line IDURA-URI-ALLOW-THROW")>]
let ``a directive names the single code after it`` (line: string) =
    Assert.Equal<string list option>(Some [ ignoreCode ], directiveCodes line)

[<Fact>]
let ``the comma-separated tail is split and trimmed`` () =
    let line =
        "// fsharpanalyzer: ignore-line IDURA-URI-ALLOW-THROW,  IDURA-OTHER-002 , the host is constant"

    Assert.Equal<string list option>(
        Some [ ignoreCode; "IDURA-OTHER-002"; "the host is constant" ],
        directiveCodes line
    )

[<Theory>]
// 'ignore-region-end' takes no codes. It is matched only so that it ends a comment block.
[<InlineData("// fsharpanalyzer: ignore-region-end")>]
// A directive with nothing after it names nothing either.
[<InlineData("// fsharpanalyzer: ignore-line")>]
let ``a directive with no tail names no codes`` (line: string) =
    Assert.Equal<string list option>(Some [], directiveCodes line)

[<Fact>]
let ``a directive followed by only whitespace names one empty code`` () =
    // Not [], because the whitespace is consumed by the pattern's separator and an empty tail
    // remains. isJustified relies on the empty token not counting as prose.
    Assert.Equal<string list option>(Some [ "" ], directiveCodes "// fsharpanalyzer: ignore-line ")
