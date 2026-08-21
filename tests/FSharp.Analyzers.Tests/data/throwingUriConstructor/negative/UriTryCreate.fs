module M

open System

// This is the safe pattern we want to use

let tryParse (input: string) : Uri option =
    match Uri.TryCreate(input, UriKind.Absolute) with
    | true, uri -> Some uri
    | false, _ -> None
