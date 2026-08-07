module Test

open System.IO
open FSharp.Control

let private readLines (path: string) : AsyncSeq<string> = asyncSeq {
    use reader = new StreamReader(path)
    let mutable line = reader.ReadLine()

    while not (isNull line) do
        yield line
        line <- reader.ReadLine()
}

// BUG: the file is opened when the caller pulls the first element,
// so the FileNotFoundException this is meant to map to empty escapes.
let linesOrEmpty (path: string) : AsyncSeq<string> =
    try
        readLines path
    with _ ->
        AsyncSeq.empty
