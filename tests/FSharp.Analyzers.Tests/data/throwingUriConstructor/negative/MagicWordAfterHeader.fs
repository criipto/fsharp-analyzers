module M

open System

// Endpoints
// Should throw because: this is a literal we control.
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let home = Uri "https://example.com"
