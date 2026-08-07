module Test

open System
open System.IO
open System.Threading

// BUG: 'cts' is disposed when the function returns the ValueTask, not when the ValueTask completes, and
// disposing a CancellationTokenSource releases its timer, so the deadline can never fire.
let read (stream: Stream) (buffer: Memory<byte>) =
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
    stream.ReadAsync(buffer, cts.Token)
