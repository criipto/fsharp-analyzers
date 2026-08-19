module M

open System

// Should throw because: the base URL comes from appsettings and is validated at
// startup by ConfigValidator, so a malformed value can never reach this call, and
// one that did would mean the validator is broken - which we want to find out
// about loudly.
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let root (baseUrl: string) = Uri baseUrl
