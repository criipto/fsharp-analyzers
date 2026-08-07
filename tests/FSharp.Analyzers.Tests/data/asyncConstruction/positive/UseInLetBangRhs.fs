module Test

open System
open System.Threading
open System.Threading.Tasks

let private get (_: CancellationToken) : Task<string> = Task.FromResult "ok"

let private getDefault () : Task<string> = Task.FromResult "ok"

// BUG: being lexically inside a task computation expression is not enough. The right-hand side of
// the let! is a plain expression, so the 'use' lowers to a real try/finally.
let fetch (timeout: TimeSpan option) = task {
    let! result =
        match timeout with
        | None -> getDefault ()
        | Some t ->
            use cts = new CancellationTokenSource(t)
            get cts.Token

    return result
}
