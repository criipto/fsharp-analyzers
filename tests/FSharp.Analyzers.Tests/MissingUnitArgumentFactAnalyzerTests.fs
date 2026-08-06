module Idura.FSharp.Analyzers.Tests.MissingUnitArgumentFactAnalyzerTests

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.MissingUnitArgumentFactAnalyzer

// Building the options restores packages, drives MSBuild and parses the resulting binlog, which
// costs about as much as running a test case. They do not depend on the program being analysed, so
// this module builds them once and every case awaits the same task.
let private options =
  lazy
    (
      mkOptionsFromProject
          "net9.0"
          [
            {
              Name = "xunit"
              Version = "2.9.3"
            }
          ]
    )

let setupContext () = Async.AwaitTask options.Value

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingUnitArgumentFactData/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"missingUnitArgumentFactData/negative"|], MemberType=typeof<TestFiles>)>]
let ``negative``(program : string, _: string) =
  runNegativeTest setupContext cliAnalyzer program
