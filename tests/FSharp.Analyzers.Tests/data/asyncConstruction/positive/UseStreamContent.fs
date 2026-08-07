module Test

open System.IO
open System.Net.Http

let private client = new HttpClient()

// BUG: the stream is handed to the request rather than read during construction, so the upload
// reads from a disposed stream while it runs.
let upload (url: string) (path: string) =
    use stream = File.OpenRead path
    client.PostAsync(url, new StreamContent(stream))
