module Test

open System
open System.Net.Http
open System.Threading
open System.Threading.Tasks

let private client = new HttpClient()

// BUG: the continuation is attached inside the 'use' scope but runs outside it, so 'cts' is
// disposed while the request it bounds is still in flight.
let status (url: string) =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    client.GetAsync(url, cts.Token).ContinueWith(fun (t: Task<HttpResponseMessage>) -> t.Result.StatusCode)
