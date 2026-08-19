module M

open System

// Should throw because: the directive below is not the one this explains.
// fsharpanalyzer: ignore-file IDURA-CRYPTO-001
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
