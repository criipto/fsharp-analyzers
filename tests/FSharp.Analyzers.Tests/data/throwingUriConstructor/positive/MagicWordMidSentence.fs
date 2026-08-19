module M

open System

// This should throw because it is convenient, or so the author claimed.
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
