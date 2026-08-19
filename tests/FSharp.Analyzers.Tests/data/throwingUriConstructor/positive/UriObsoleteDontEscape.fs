module M

open System

#nowarn "44"

let parse (input: string) : Uri = Uri(input, true)
