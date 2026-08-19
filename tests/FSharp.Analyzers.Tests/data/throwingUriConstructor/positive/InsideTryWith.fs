module M

open System

// Catching the exception is still worse than not throwing in the first place, so this is reported.
let parse (input: string) : Uri option =
    try
        Some(Uri input)
    with :? UriFormatException ->
        None
