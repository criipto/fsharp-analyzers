module Idura.FSharp.Analyzers.Tests.DoNotUseYourOwnRandomAnalyzer

open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.DoNotUseYourOwnRandomAnalyzer

let setupContext () = projectOptions "net10.0" []

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"doNotUseYourOwnRandom/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program
