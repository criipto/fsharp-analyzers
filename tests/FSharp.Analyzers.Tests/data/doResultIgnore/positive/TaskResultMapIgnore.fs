module M

open FsToolkit.ErrorHandling

let validateSubfunction (input : string) =
    if (input <> "abc") then
        TaskResult.error "Error"
    else
        TaskResult.ofResult (Result.Ok input)

let validate (input : string) = taskResult {
    return validateSubfunction input
}

let main : TaskResult<unit,string> = taskResult {
    let a = "def"
    do! validate a |> TaskResult.map ignore
    return ()
}