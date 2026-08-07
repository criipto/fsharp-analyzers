module Test

open System
open System.Net.Http
open System.Threading

let private client = new HttpClient()

// BUG: as UseCtsReturnedTask, but the computation is an Async<_>. For an Async the disposal is
// even further from the work: nothing runs until the value is started.
let fetch (url: string) : Async<string> =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    client.GetStringAsync(url, cts.Token) |> Async.AwaitTask
