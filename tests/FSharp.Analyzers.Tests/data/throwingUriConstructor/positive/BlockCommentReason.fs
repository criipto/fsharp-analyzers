module M

open System

(* Should throw because: the SDK only honours line comments, so this does not count. *)
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
