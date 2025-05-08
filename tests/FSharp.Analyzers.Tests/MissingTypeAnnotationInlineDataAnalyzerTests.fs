module Criipto.FSharp.Analyzers.Tests.MissingTypeAnnotationInlineDataAnalyzerTests

open System.IO

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Criipto.FSharp.Analyzers.MissingTypeAnnotationInlineDataAnalyzer

let setupContext () = async {
  let! opts =
          mkOptionsFromProject
              "netstandard2.0" // ensures compatibility with both .NET Framework and newer .NET versions
              [
                {
                  Name = "xunit"
                  Version = "2.9.3"
                }
              ]
          |> Async.AwaitTask
  return opts
}

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
  let! opts = setupContext()
  let program = TestFiles.GetSource "missingTypeAnnotationInlineData/positive/IgnoresMultipleInlineData.fs"
  let ctx = getContext opts program
  let! msgs = cliAnalyzer ctx
  Assert.True(msgs.Length = 1)
}

[<Fact>]
let ``only gives one warning on multiple InlineData attributes with two parameters``() = async {
  let! opts = setupContext()
  let program = TestFiles.GetSource "missingTypeAnnotationInlineData/positive/IgnoresMultipleInlineDataTwo.fs"
  let ctx = getContext opts program
  let! msgs = cliAnalyzer ctx
  Assert.True(msgs.Length = 1)
}

[<Fact>]
let ``gives two warnings for two parameters with missing types``() = async {
  let! opts = setupContext()
  let program = TestFiles.GetSource "missingTypeAnnotationInlineData/positive/TwoMissing.fs"
  let ctx = getContext opts program
  let! msgs = cliAnalyzer ctx
  Assert.True(msgs.Length = 2)
}

[<Fact>]
let ``gives one warning for one parameter missing types``() = async {
  let! opts = setupContext()
  let program = TestFiles.GetSource "missingTypeAnnotationInlineData/positive/TwoInts.fs"
  let ctx = getContext opts program
  let! msgs = cliAnalyzer ctx
  Assert.True(msgs.Length = 1)
}