module Test

open FsToolkit.ErrorHandling

let private fetch () : Async<Result<string, string>> = AsyncResult.ok "ok"

// CORRECT: the await happens inside the try, so the asyncResult builder's TryWith wraps
// execution and the handler observes faults raised while the underlying Async runs.
let run () : Async<Result<string, string>> = asyncResult {
    try
        return! fetch ()
    with _ -> return "fallback"
}
