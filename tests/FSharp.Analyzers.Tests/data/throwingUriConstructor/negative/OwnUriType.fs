module M

// This is a type called Uri, but it is not System.Uri (and same for UriBuilder)

type Uri = { Value: string }
type UriBuilder = { Parts: string list }

let ours : Uri = { Value = "not System.Uri" }
let builder : UriBuilder = { Parts = [ "also"; "not" ] }
