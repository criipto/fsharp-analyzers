module Idura.FSharp.Analyzers.Tests.MissingUnitArgumentFactAnalyzerTests

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.MissingUnitArgumentFactAnalyzer

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
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingUnitArgumentFactData/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingUnitArgumentFactData/negative"|], MemberType=typeof<TestFiles>)>]
let ``negative``(program : string, _: string) =
  runNegativeTest setupContext cliAnalyzer program
