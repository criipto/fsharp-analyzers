module M

open System

// Should throw because: every URI in this region is a literal we control.
// fsharpanalyzer: ignore-region-start IDURA-URI-ALLOW-THROW
let home = Uri "https://example.com"
let docs = Uri "https://example.com/docs"
let builder = UriBuilder "https://example.com/api"
// fsharpanalyzer: ignore-region-end
