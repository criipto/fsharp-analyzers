module Idura.FSharp.Analyzers.Tests.DoubleWrappedResultAnalyzer

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.DoubleWrappedResultAnalyzer

let setupContext () = async {
  let! opts =
          mkOptionsFromProject
              "net9.0"
              [
                {
                  Name = "FsToolkit.Errorhandling"
                  Version = "5.1.0"
                }
              ]
          |> Async.AwaitTask
  return opts
}

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
  let! opts = setupContext()
  let program = TestFiles.GetSource "doubleWrappedResult/positive/TripleResult.fs"
  let ctx = getContext opts program
  let! msgs = cliAnalyzer ctx
  Assert.True(msgs.Length = 3)
}