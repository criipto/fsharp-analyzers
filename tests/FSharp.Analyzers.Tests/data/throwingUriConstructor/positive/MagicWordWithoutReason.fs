module M

open System

// Should throw because:
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
