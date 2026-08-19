module M

open System

let parse (input: string) : Uri = Uri(input, UriKind.Absolute)
