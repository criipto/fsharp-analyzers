module Test

open System.IO
open FSharp.Control

let private readLines (reader: StreamReader) : AsyncSeq<string> = asyncSeq {
    let mutable line = reader.ReadLine()

    while not (isNull line) do
        yield line
        line <- reader.ReadLine()
}

// BUG: an AsyncSeq runs nothing when it is constructed, so the reader is disposed before a single
// line has been pulled and every element fails.
let lines (path: string) : AsyncSeq<string> =
    use reader = new StreamReader(path)
    readLines reader
