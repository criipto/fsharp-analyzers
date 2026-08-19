module M

open System

// Note spaces on next comment lines
// Should throw because:       
//    
//   
// fsharpanalyzer: ignore-line-next IDURA-URI-ALLOW-THROW
let parse (input: string) = Uri input
