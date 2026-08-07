module Test

open FsToolkit.ErrorHandling

let private fetch () : TaskResult<string, string> = TaskResult.ok "ok"

// CORRECT: the await happens inside the try, so the taskResult builder's TryWith wraps
// execution and the handler observes faults raised while the underlying Task runs.
let run () : TaskResult<string, string> = taskResult {
    try
        return! fetch ()
    with _ -> return "fallback"
}
