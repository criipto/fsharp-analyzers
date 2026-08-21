module M

open System

// The SDK matches the prefix case-sensitively, so this is not a directive at all.
// FSharpAnalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
