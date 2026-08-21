module M

open System

// This is not a constructor, it's just using the type

type Endpoint = { Address: Uri; Name: string }

let addressOf (endpoint: Endpoint) : Uri = endpoint.Address
