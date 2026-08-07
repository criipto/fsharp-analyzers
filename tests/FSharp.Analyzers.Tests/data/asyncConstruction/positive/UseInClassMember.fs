module Test

open System.IO
open System.Net.Http
open System.Threading.Tasks

let private client = new HttpClient()

// BUG: the stream is handed to the request rather than read during construction, so the upload
// reads from a disposed stream while it runs.
type Uploader(url: string) =
    member _.Upload(path: string) : Task<HttpResponseMessage> =
        use stream = File.OpenRead path
        client.PostAsync(url, new StreamContent(stream))
