module M

// Should throw because: this file has nothing but literal endpoints
// fsharpanalyzer: ignore-file IDURA-URI-ALLOW-THROW

open System

let home = Uri "https://example.com"
let docs = UriBuilder "https://example.com/docs"
