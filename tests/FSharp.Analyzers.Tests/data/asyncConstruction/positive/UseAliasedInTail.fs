module Test

open System
open System.IO
open System.Net.Http
open System.Threading

let private client = new HttpClient()

// BUG: UseCtsReturnedTask with one 'let' in between. The token is a handle on the source rather than
// a copy of anything, so the deadline still dies with the 'use'.
let fetch (url: string) =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    let token = cts.Token
    client.GetAsync(url, token)

// BUG: UseStreamContent with one 'let' in between. StreamContent reads the stream when the request
// is sent, which is after this function has closed it.
let upload (url: string) (path: string) =
    use stream = File.OpenRead path
    let content = new StreamContent(stream)
    client.PostAsync(url, content)

// BUG: Disposing 'cts' disposes what 'source' names.
let fetchAliased (url: string) =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    let source = cts
    client.GetAsync(url, source.Token)
