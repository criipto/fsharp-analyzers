module Test

open System
open System.Net.Http
open System.Threading

let private client = new HttpClient()

// CORRECT: a 'use' in a computation expression goes through the builder's Using member, so no
// user-level try/finally appears and the scope correctly spans the awaits.
let fetch (url: string) : Async<int> = async {
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    let! body = client.GetStringAsync(url, cts.Token) |> Async.AwaitTask
    return body.Length
}
