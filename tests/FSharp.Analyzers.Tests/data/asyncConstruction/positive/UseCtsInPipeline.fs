module Test

open System
open System.Net.Http
open System.Threading
open System.Threading.Tasks

let private client = new HttpClient()

let private onTrack (_: string) (t: Task<HttpResponseMessage>) : Task<HttpResponseMessage> = t

// BUG: as UseCtsReturnedTask, but 'cts' is referenced by the leftmost element of a pipeline, so
// finding it needs a search over the whole tail rather than a match on its head.
let fetch (url: string) =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    client.GetAsync(url, cts.Token)
    |> onTrack "Couldn't get"
