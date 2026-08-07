module Test

open System
open System.Threading

let private get (_: CancellationToken) : string = "ok"

// CORRECT: Nothing asynchronous is constructed, so the 'use' scope already covers the whole computation.
let fetch (timeout: TimeSpan option) =
    match timeout with
    | None -> get CancellationToken.None
    | Some t ->
        use cts = new CancellationTokenSource(t)
        get cts.Token
