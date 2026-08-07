module Test

open FsToolkit.ErrorHandling

let private fetch () : TaskResult<string, string> = TaskResult.ok "ok"

// BUG: a non-CE function returns the try/with value directly.
// The caller awaits it, so the handler guards construction only.
let run () : TaskResult<string, string> =
    try
        fetch ()
    with _ -> reraise ()
