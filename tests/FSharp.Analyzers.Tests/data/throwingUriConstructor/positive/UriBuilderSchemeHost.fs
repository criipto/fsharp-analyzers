module M

open System

let build (scheme: string) (host: string) : UriBuilder = UriBuilder(scheme, host)
