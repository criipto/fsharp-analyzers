module Test

open System.IO
open System.Net.Http
open System.Threading.Tasks

let private client = new HttpClient()

type IUploader =
    abstract Upload: string -> Task<HttpResponseMessage>

type IMirror =
    abstract Mirror: string -> Task<HttpResponseMessage>

// BUG, twice: UseStreamContent inside an object expression. The untyped-tree traversal that decides
// which message a finding gets has to reach both the 'members' field, where the parser puts members
// written after 'with', and the 'extraImpls' of the added interface.
let uploader (url: string) =
    { new IUploader with
        member _.Upload(path: string) =
            use stream = File.OpenRead path
            client.PostAsync(url, new StreamContent(stream))

      interface IMirror with
          member _.Mirror(path: string) =
              use stream = File.OpenRead path
              client.PostAsync(url + "/mirror", new StreamContent(stream)) }
