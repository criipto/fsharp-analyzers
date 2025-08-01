module Criipto.FSharp.Analyzers.Tests.MissingUnitArgumentFactAnalyzerTests

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Criipto.FSharp.Analyzers.MissingUnitArgumentFactAnalyzer

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
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingUnitArgumentFactData/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingUnitArgumentFactData/negative"|], MemberType=typeof<TestFiles>)>]
let ``negative``(program : string, _: string) =
  runNegativeTest setupContext cliAnalyzer program
