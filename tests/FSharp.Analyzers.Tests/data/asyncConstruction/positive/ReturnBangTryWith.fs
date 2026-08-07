module Test

open System.Threading.Tasks

let private fetch () : Task<string> = Task.FromResult "ok"

// BUG: the try/with wraps construction of the Task.
// return! awaits it outside the handler, so a fault raised while the Task runs is never observed here.
let run () : Task<string> = task {
    return!
        try
            fetch ()
        with _ -> reraise ()
}
