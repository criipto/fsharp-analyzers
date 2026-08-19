module M

open System

// should throw because: the URL is a literal we control.
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let home = Uri "https://example.com"
