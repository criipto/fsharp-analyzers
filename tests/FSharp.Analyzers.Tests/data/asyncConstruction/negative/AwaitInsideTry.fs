module Test

open System.Threading.Tasks

let private fetch () : Task<string> = Task.FromResult "ok"

// CORRECT: the await happens inside the try, so the computation expression's TryWith
// wraps execution and the handler observes faults raised while the Task runs.
let run () : Task<string> = task {
    try
        return! fetch ()
    with _ -> return "fallback"
}
