module M

open System

// Should throw because: the blank line below means this never reaches the directive.

// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
