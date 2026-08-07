module Test

open System.IO
open System.Net.Http

let private client = new HttpClient()

// CORRECT: the only reference to the reader is in a binding sequenced before the tail expression, so
// it is finished with by the time the function returns. This is the case the "referenced by the tail
// expression" condition exists for.
let upload (url: string) (path: string) =
    use reader = new StreamReader(path)
    let body = reader.ReadToEnd()
    client.PostAsync(url, new StringContent(body))
