module M

open System

// A section header, or any other comment that was never meant as a justification.
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
