module M

// U is an alias of the constructor, but it still throws, so we should still detect it

type U = System.Uri

let parse (input: string) : U = U input
