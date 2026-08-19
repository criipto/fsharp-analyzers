module M

open System

// Should throw because: every URI in this region is a literal we control.
// fsharpanalyzer: ignore-region-start IDURA-URI-ALLOW-THROW
let home = Uri "https://example.com"
// fsharpanalyzer: ignore-region-end

let parse (input: string) = Uri input
