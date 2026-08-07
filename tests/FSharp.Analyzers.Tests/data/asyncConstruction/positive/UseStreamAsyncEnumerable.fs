module Test

open System.Collections.Generic
open System.IO
open System.Text.Json

// BUG: Deserializing reads from the stream as elements are pulled,
// which is after this function has closed the file.
let ids (path: string) : IAsyncEnumerable<int> =
    use stream = File.OpenRead path
    JsonSerializer.DeserializeAsyncEnumerable<int> stream
