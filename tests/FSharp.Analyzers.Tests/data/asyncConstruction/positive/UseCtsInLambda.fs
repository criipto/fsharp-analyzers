module Test

open System
open System.Net.Http
open System.Threading
open System.Threading.Tasks

let private client = new HttpClient()

let private bindAsync (f: string -> Task<string>) (t: Task<string>) : Task<string> = task {
    let! x = t
    return! f x
}

// BUG: the enclosing function is not asynchronously typed, so the walk
// has to descend into lambdas to find this.
let fetch (url: string) =
    Task.FromResult url
    |> bindAsync (fun u ->
        use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
        client.GetStringAsync(u, cts.Token))
