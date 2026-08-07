module Test

open System
open System.Net.Http
open System.Threading

let private client = new HttpClient()

// BUG: 'cts' is disposed when the function returns the Task, not when the Task completes, and
// disposing a CancellationTokenSource releases its timer, so the deadline can never fire.
let fetch (url: string) =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    client.GetAsync(url, cts.Token)
