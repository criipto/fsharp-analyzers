module Test

open System
open System.Threading

open FsToolkit.ErrorHandling

let private get (_: CancellationToken) : TaskResult<string, string> = TaskResult.ok "ok"

// CORRECT:  a 'use' in a computation expression goes through the builder's Using member, so no
// user-level try/finally appears and the scope correctly spans the awaits.
// The type test resolves the abbreviation through to a Task,
// but the builder's Using member is still what handles the disposal.
let fetch () : TaskResult<string, string> = taskResult {
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    let! body = get cts.Token
    return body
}
