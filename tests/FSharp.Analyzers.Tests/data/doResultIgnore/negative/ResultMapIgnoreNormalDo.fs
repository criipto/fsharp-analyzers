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

let main : Result<unit,string> = result {
    let a = "def"
    do validate a |> Result.map ignore
    return ()
}