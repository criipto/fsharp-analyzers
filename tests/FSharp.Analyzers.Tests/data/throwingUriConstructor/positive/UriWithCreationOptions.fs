module M

open System

// The overload takes the options by inref, so they have to be bound first.
let parse (input: string) : Uri =
    let mutable options = UriCreationOptions()
    Uri(input, &options)
