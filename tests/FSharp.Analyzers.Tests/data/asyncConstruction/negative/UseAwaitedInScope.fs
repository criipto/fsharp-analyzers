module Test

open System
open System.Net.Http
open System.Threading
open System.Threading.Tasks

let private client = new HttpClient()

// CORRECT for this analyzer: the request has already completed inside the 'use' scope, so the disposal is
// correctly ordered. Blocking on Wait is a separate problem.
let fetch (url: string) : Task<string> =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    let t = client.GetStringAsync(url, cts.Token)
    t.Wait()
    t
