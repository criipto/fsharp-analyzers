module M

open FsToolkit.ErrorHandling

let validateSubfunction (input : string) =
    if (input <> "abc") then
        Result.Error "Error"
    else
        Result.Ok input

let validate (input : string) = result {
    return! validateSubfunction input
}

let main : Result<string, string> = result {
    let a = "def"
    let b = validate a

    // This should give a warning
    let validate2 input = result {
        return Result.bind validate input
    }

    return! b
}