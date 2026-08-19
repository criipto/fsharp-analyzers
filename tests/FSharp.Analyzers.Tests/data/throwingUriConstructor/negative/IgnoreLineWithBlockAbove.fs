module M

open System

// Should throw because: this is a literal we control, so it cannot be malformed at run time.
let home = Uri "https://example.com" // fsharpanalyzer: ignore-line IDURA-URI-ALLOW-THROW
