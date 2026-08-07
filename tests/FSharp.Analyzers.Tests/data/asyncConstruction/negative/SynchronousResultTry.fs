module Test

open FsToolkit.ErrorHandling

// CORRECT: the same "await outside the try" shape as the bug, but the try wraps synchronous work
// producing a Result, so the fault is thrown and caught here.
let parse (s: string) : Result<int, string> =
    result {
        return!
            try
                Ok(int s)
            with _ -> Error "not an int"
    }
