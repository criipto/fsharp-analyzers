module Test

open System.Net.Http
open System.Threading
open System.Threading.Tasks

let private client = new HttpClient()

// CORRECT: the 'try' is part of a computation expression, so the 'finally' runs
// when the computation completes.
let fetch (sem: SemaphoreSlim) (url: string) = task {
    sem.Wait()

    try
        return! client.GetStringAsync url
    finally
        sem.Release() |> ignore
}

// The same shape for an Async
let fetchAsync (sem: SemaphoreSlim) (url: string) : Async<string> = async {
    sem.Wait()

    try
        return! client.GetStringAsync url |> Async.AwaitTask
    finally
        sem.Release() |> ignore
}

// A try/finally that is not over an asynchronous value at all: the computation is awaited inside
// the 'try', so the 'finally' already runs after it completes.
let fetchLength (sem: SemaphoreSlim) (url: string) : Task<int> = task {
    sem.Wait()

    try
        let! body = client.GetStringAsync url
        return body.Length
    finally
        sem.Release() |> ignore
}
