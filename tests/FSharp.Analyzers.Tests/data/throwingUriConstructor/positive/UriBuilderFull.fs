module M

open System

let build (host: string) (path: string) (query: string) : UriBuilder =
    UriBuilder("https", host, 443, path, query)
