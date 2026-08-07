module Test

open FsToolkit.ErrorHandling

let private fetch () : Async<Result<string, string>> = AsyncResult.ok "ok"

// BUG: a non-CE function returns the try/with value directly.
// The caller awaits it, so the handler guards construction only.
let run () : Async<Result<string, string>> =
    try
        fetch ()
    with _ -> reraise ()
