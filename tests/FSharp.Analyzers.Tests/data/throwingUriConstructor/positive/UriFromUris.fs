module M

open System

let combine (baseUri: Uri) (relative: Uri) : Uri = Uri(baseUri, relative)
