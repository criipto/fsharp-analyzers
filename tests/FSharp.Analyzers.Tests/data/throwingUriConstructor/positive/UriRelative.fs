module M

open System

let combine (baseUri: Uri) (path: string) : Uri = Uri(baseUri, path)
