module Test

open System.Threading
open System.Threading.Tasks

let private acquire () : Task<CancellationTokenSource> =
    Task.FromResult (new CancellationTokenSource())

let private get (_: CancellationToken) : Task<string> = Task.FromResult "ok"

// CORRECT: use! goes through the builder's Using member as well.
let run () = task {
    use! cts = acquire ()
    return! get cts.Token
}
