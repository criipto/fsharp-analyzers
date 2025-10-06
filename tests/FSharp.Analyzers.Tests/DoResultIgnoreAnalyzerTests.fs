module Idura.FSharp.Analyzers.Tests.DoResultIgnoreAnalyzerTests

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.DoResultIgnoreAnalyzer

let setupContext () = async {
  let! opts =
          mkOptionsFromProject
              "netstandard2.0" // ensures compatibility with both .NET Framework and newer .NET versions
              [
                {
                  Name = "FsToolkit.Errorhandling"
                  Version = "5.0.0"
                }
              ]
          |> Async.AwaitTask
  return opts
}

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"doResultIgnore/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"doResultIgnore/negative"|], MemberType=typeof<TestFiles>)>]
let ``negative``(program : string, _: string) =
  runNegativeTest setupContext cliAnalyzer program
