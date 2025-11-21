module M

open FsToolkit.ErrorHandling

// This function has type: string -> Result<string, string>
let validateSubfunction (input : string) =
    if (input <> "abc") then
        Result.Error "Error"
    else
        Result.Ok input

// This function has type: string -> Result<Result<string, string>, 'a>
let validate (input : string) = result {
    return validateSubfunction input
}

let main : Result<unit,string> = result {
    let a = "def"
    // The next line has value `Result.Error "Error"`, but this is not caught due to the double-wrapping in `validate`.
    // This means that validation result is completely ignored.
    let! _ = validate a
    return ()
}