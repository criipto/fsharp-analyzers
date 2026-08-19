module M

open System

let parse (input: string) = Uri input // fsharpanalyzer: ignore-line IDURA-URI-ALLOW-THROW
