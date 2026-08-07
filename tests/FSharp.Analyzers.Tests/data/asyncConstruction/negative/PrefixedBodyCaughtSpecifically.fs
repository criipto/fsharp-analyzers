module Test

open System
open System.IO
open System.Net.Http
open System.Threading.Tasks

let private client = new HttpClient()

// Takes ownership of the stream, so the caller does not need a 'use'.
let private readAllAndClose (stream: Stream) : Task<string> = task {
    use owned = stream
    use reader = new StreamReader(owned)
    return! reader.ReadToEndAsync()
}

// CORRECT: the UriFormatException is raised while the 'try' is still running, and the handler does
// catch it. A handler naming one exception type cannot be told apart from one written for the
// request, so a body guarding synchronous work as well is reported only when the handler catches
// everything - see positive/PrefixedBodyCaughtBroadly, which is this body with a catch-all.
let fetch (url: string) : Task<string> =
    try
        let uri = Uri url
        client.GetStringAsync uri
    with :? UriFormatException ->
        Task.FromResult ""

// CORRECT: the same for the open-then-wrap shape, where the synchronous step is what raises.
let read (path: string) : Task<string> =
    try
        let stream = File.OpenRead path
        readAllAndClose stream
    with :? FileNotFoundException ->
        Task.FromResult ""

// CORRECT: a branch counts as guarded work too, because the matched
// expression is evaluated inside the 'try'.
// This one cannot raise, but it is not easily decidable which branches can.
let cached (cache: Map<string, string>) (url: string) : Task<string> =
    try
        match Map.tryFind url cache with
        | Some body -> Task.FromResult body
        | None -> client.GetStringAsync url
    with :? HttpRequestException ->
        Task.FromResult ""
