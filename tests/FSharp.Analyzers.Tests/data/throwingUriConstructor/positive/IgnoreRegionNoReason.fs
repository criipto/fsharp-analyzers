module M

open System

// fsharpanalyzer: ignore-region-start IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
let build (input: string) = UriBuilder input
// fsharpanalyzer: ignore-region-end
