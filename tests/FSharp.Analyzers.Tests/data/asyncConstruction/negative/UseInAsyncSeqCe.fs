module Test

open System
open System.Threading

open FSharp.Control

let private get (_: CancellationToken) : Async<string> = async.Return "ok"

// CORRECT: a 'use' in a computation expression goes through the builder's Using member, so no
// user-level try/finally appears and the scope correctly spans the awaits.
let items () = asyncSeq {
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    let! first = get cts.Token
    yield first
    let! second = get cts.Token
    yield second
}
