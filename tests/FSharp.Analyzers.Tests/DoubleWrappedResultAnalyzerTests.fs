module Idura.FSharp.Analyzers.Tests.DoubleWrappedResultAnalyzer

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.DoubleWrappedResultAnalyzer

let setupContext () =
  projectOptions
      "net10.0"
      [
        {
          Name = "FsToolkit.Errorhandling"
          Version = "5.1.0"
        }
      ]

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"doubleWrappedResult/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"doubleWrappedResult/negative"|], MemberType=typeof<TestFiles>)>]
let ``negative``(program : string, _: string) =
  runNegativeTest setupContext cliAnalyzer program

[<Fact>]
let ``gives three warnings for TripleResult``() = async {
  let! msgs = messagesFor setupContext cliAnalyzer "doubleWrappedResult/positive/TripleResult.fs"
  Assert.True(msgs.Length = 3)
}