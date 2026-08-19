module M

open System

// Should throw because: this is a literal we control, so it cannot be malformed at run time.
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let home = Uri "https://example.com"
