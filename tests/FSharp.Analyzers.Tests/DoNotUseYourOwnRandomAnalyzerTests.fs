module Idura.FSharp.Analyzers.Tests.DoNotUseYourOwnRandomAnalyzer

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.DoNotUseYourOwnRandomAnalyzer

let setupContext () = async {
  let! opts =
          mkOptionsFromProject
              "net9.0"
              []
          |> Async.AwaitTask
  return opts
}

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"doNotUseYourOwnRandom/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program
