module Test

// CORRECT: an ordinary synchronous try/with; nothing asynchronous is involved.
let parse (s: string) : int =
    try
        int s
    with _ -> -1
