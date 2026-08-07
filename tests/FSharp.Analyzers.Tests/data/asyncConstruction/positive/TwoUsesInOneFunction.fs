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

// BUG, twice: every 'use' has to be reported, not just the first. The second sits inside a lambda,
// so the two findings are reached by different paths through the tree.
let fetch (url: string) =
    use getCts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    client.GetStringAsync(url, getCts.Token)
    |> bindAsync (fun body ->
        use putCts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
        client.GetStringAsync(url + body, putCts.Token))
