module M

open FsToolkit.ErrorHandling

let makeResource name =
   { new System.IDisposable
     with member this.Dispose() = printfn "%s disposed" name }
    |> Result.Ok

let main : Result<unit,string> = result {
    let a = "def"
    use! _ = makeResource "a"
    return ()
}