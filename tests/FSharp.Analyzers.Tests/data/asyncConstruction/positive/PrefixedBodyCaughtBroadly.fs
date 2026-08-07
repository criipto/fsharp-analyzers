module Test

open System.Net.Http
open System.Threading
open System.Threading.Tasks

let private client = new HttpClient()

// BUG: the 'try' guards a binding as well as the construction, so it could have been written for the
// binding - but nothing the StringContent constructor raises explains a catch-all, so what this
// handler is for is the request's own faults, delivered outside it.
// Compare negative/PrefixedBodyCaughtSpecifically, which is the same body with a handler
// naming only one exception type.
let post (url: string) (payload: string) : Task<HttpResponseMessage> =
    try
        let content = new StringContent(payload)
        client.PostAsync(url, content)
    with _ ->
        Task.FromResult(new HttpResponseMessage())

// BUG: the same, with a statement rather than a binding in front of the construction, and a named
// rather than a wildcard catch-all.
let guardedFetch (sem: SemaphoreSlim) (url: string) : Task<string> =
    try
        sem.Wait()
        client.GetStringAsync url
    with ex ->
        Task.FromResult ex.Message
