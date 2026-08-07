module Test

open System
open System.Net.Http
open System.Threading

let private client = new HttpClient()

// CORRECT: the 'use' spans execution rather than construction.
let fetch (url: string) = task {
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    return! client.GetStringAsync(url, cts.Token)
}
