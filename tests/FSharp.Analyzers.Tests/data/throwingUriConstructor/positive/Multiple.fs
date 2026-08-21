module M

open System

let first (input: string) = Uri input
let second (input: string) = UriBuilder input
let third (input: string) = Uri(input, UriKind.Relative)
