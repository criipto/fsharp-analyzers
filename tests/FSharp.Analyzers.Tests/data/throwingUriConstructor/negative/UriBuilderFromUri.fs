module M

open System

// This constructor does not throw if the Uri is already validated

let build (uri: Uri) : UriBuilder = UriBuilder uri
