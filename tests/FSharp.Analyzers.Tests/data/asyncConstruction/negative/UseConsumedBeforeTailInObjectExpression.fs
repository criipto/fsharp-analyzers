module Test

open System.IO
open System.Net.Http
open System.Threading.Tasks

let private client = new HttpClient()

type IUploader =
    abstract Upload: string -> Task<HttpResponseMessage>

type IMirror =
    abstract Mirror: string -> Task<HttpResponseMessage>

// CORRECT: UseConsumedBeforeTail written inside an object expression.
let uploader (url: string) =
    { new IUploader with
        member _.Upload(path: string) =
            use reader = new StreamReader(path)
            let body = reader.ReadToEnd()
            client.PostAsync(url, new StringContent(body))

      interface IMirror with
          member _.Mirror(path: string) =
              use reader = new StreamReader(path)
              let body = reader.ReadToEnd()
              client.PostAsync(url + "/mirror", new StringContent(body)) }

// CORRECT: the same in a class member.
type Uploader(url: string) =
    member _.Upload(path: string) : Task<HttpResponseMessage> =
        use reader = new StreamReader(path)
        let body = reader.ReadToEnd()
        client.PostAsync(url, new StringContent(body))
