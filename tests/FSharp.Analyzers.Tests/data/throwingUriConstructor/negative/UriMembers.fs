module M

open System

// This is not a constructor, it's just usage of the Uri class

let describe (uri: Uri) = $"%s{uri.Scheme}://%s{uri.Host}%s{uri.AbsolutePath}"
