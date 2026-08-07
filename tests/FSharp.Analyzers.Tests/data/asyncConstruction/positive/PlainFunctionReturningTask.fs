module Test

open System.Threading.Tasks

let private fetch () : Task<string> = Task.FromResult "ok"

// BUG: a non-CE function returns the try/with value directly.
// The caller awaits it, so the handler guards construction only.
let run () : Task<string> =
    try
        fetch ()
    with _ -> reraise ()
