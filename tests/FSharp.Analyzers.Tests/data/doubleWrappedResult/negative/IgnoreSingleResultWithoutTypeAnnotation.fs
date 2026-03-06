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

let main = result {
    let a = "def"
    let! b = validate a
    if true then
        return b
    else
        return! Result.Error "check it out"
}