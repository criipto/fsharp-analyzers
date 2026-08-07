module Idura.FSharp.Analyzers.Tests.AsyncConstructionAnalyzerTests

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing
open TestHelpers

open Xunit
open Snapshooter
open Snapshooter.Xunit

open Idura.FSharp.Analyzers.AsyncConstructionAnalyzer

// Building the options costs about as much as running a test case, and does not depend on the
// program being analysed, so every case awaits the same task.
let private options =
  lazy
    (
      // The SDK.Testing harness cannot construct a net48 project context, so we target net9.0 like
      // the other analyzers whose sample programs need full async support. The typed trees this
      // analyzer keys on are framework-independent.
      mkOptionsFromProject
          "net9.0"
          [
            {
              Name = "FsToolkit.Errorhandling"
              Version = "5.0.0"
            }
            {
              Name = "FSharp.Control.AsyncSeq"
              Version = "3.2.1"
            }
          ]
    )

let setupContext () = Async.AwaitTask options.Value

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"asyncConstruction/positive"|], MemberType=typeof<TestFiles>)>]
let ``positive``(program : string, filename: string) =
  let snapshotName = Snapshot.FullName(SnapshotNameExtension.Create filename)
  runPositiveTest snapshotName setupContext cliAnalyzer program

[<Theory>]
[<MemberData(nameof(TestFiles.GetSources), parameters=[|"asyncConstruction/negative"|], MemberType=typeof<TestFiles>)>]
let ``negative``(program : string, _: string) =
  runNegativeTest setupContext cliAnalyzer program

let private messages (file: string) = messagesFor setupContext cliAnalyzer file

// This analyzer reports two rules, so the tests below pin which shape belongs to each rule

[<Fact>]
let ``a try with over a Task is reported once, as IDURA-ASYNC-001``() = async {
  let! msgs = messages "asyncConstruction/positive/PlainFunctionReturningTask.fs"
  let msg = Assert.Single msgs
  Assert.Equal(tryConstructionCode, msg.Code)
  Assert.Equal(tryConstructionName, msg.Type)
}

[<Fact>]
let ``a hand-written try finally over a Task is reported once, as IDURA-ASYNC-002``() = async {
  let! msgs = messages "asyncConstruction/positive/TryFinallyOverTask.fs"
  let msg = Assert.Single msgs
  Assert.Equal(resourceLifetimeCode, msg.Code)
  Assert.Equal(resourceLifetimeName, msg.Type)
}

// A 'use' lowers to a Let wrapping a try/finally. The finding is recognised at the Let, so the
// try/finally underneath it must not also be reported as a hand-written one.
[<Fact>]
let ``a use is reported once, not also as a hand-written try finally``() = async {
  let! msgs = messages "asyncConstruction/positive/UseCtsReturnedTask.fs"
  let msg = Assert.Single msgs
  Assert.Equal(resourceLifetimeCode, msg.Code)
}

// A lowered 'use' that cannot be matched back to a binding in the untyped tree falls through to the
// hand-written try/finally arm, which reports a 'try' the developer never wrote.
[<Fact>]
let ``a use inside an object expression is reported as a use, not as a hand-written try finally``() = async {
  let! msgs = messages "asyncConstruction/positive/UseInObjectExpression.fs"
  Assert.Equal(2, List.length msgs)
  Assert.All(msgs, fun m -> Assert.Equal(resourceLifetimeCode, m.Code))
  // Only the 'use' variant of the message can name the resource.
  Assert.All(msgs, fun m -> Assert.Contains("This 'use' disposes 'stream'", m.Message))
}

// Every other 001 sample has a body simple enough not to need the handler, so this is the only case
// that pins the match between the untyped-tree handler and the typed 'try'.
[<Fact>]
let ``a body that guards synchronous work is reported when the handler catches everything``() = async {
  let! msgs = messages "asyncConstruction/positive/PrefixedBodyCaughtBroadly.fs"
  Assert.Equal(2, List.length msgs)
  Assert.All(msgs, fun m -> Assert.Equal(tryConstructionCode, m.Code))
}

[<Fact>]
let ``every use in a function is reported, including one inside a lambda``() = async {
  let! msgs = messages "asyncConstruction/positive/TwoUsesInOneFunction.fs"
  Assert.Equal(2, List.length msgs)
  Assert.All(msgs, fun m -> Assert.Equal(resourceLifetimeCode, m.Code))
  // Distinct ranges to check that this is two 'use' bindings rather than one reported twice.
  Assert.Equal(2, msgs |> List.map (fun m -> m.Range) |> List.distinct |> List.length)
  Assert.Contains("'getCts'", (List.head msgs).Message)
  Assert.Contains("'putCts'", (List.last msgs).Message)
}
