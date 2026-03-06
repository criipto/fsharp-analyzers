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

let validate2 input = result {
    return Result.bind validate input
}

let main : Result<Result<Result<string, string>, string>, string> = result {
    let a = "def"
    let b = validate a
    let c = validate2 b
    return c
}