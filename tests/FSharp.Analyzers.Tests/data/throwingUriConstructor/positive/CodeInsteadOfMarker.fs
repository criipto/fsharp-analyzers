module M

open System

// This names a code the analyzer never emits and never reads, so it suppresses nothing.
// fsharpanalyzer: ignore-line-next IDURA-URI-002
let parse (input: string) = Uri input
