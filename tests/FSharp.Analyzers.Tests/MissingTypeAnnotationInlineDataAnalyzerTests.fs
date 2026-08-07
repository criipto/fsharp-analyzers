module Idura.FSharp.Analyzers.Tests.MissingTypeAnnotationInlineDataAnalyzerTests

open System.IO

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.MissingTypeAnnotationInlineDataAnalyzer

let setupContext () =
  projectOptions
      "net10.0"
      [
        {
          Name = "xunit"
          Version = "2.9.3"
        }
      ]

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingTypeAnnotationInlineData/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingTypeAnnotationInlineData/negative"|], MemberType=typeof<TestFiles>)>]
let ``negative``(program : string, _: string) =
  runNegativeTest setupContext cliAnalyzer program

[<Fact>]
let ``only gives one warning on multiple InlineData attributes``() = async {
  let! msgs = messagesFor setupContext cliAnalyzer "missingTypeAnnotationInlineData/positive/IgnoresMultipleInlineData.fs"
  Assert.True(msgs.Length = 1)
}

[<Fact>]
let ``only gives one warning on multiple InlineData attributes with two parameters``() = async {
  let! msgs = messagesFor setupContext cliAnalyzer "missingTypeAnnotationInlineData/positive/IgnoresMultipleInlineDataTwo.fs"
  Assert.True(msgs.Length = 1)
}

[<Fact>]
let ``gives two warnings for two parameters with missing types``() = async {
  let! msgs = messagesFor setupContext cliAnalyzer "missingTypeAnnotationInlineData/positive/TwoMissing.fs"
  Assert.True(msgs.Length = 2)
}

[<Fact>]
let ``gives one warning for one parameter missing types``() = async {
  let! msgs = messagesFor setupContext cliAnalyzer "missingTypeAnnotationInlineData/positive/TwoInts.fs"
  Assert.True(msgs.Length = 1)
}