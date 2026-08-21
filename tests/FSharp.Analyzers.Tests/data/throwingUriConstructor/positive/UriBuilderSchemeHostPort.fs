module M

open System

let build (host: string) (port: int) : UriBuilder = UriBuilder("https", host, port)
