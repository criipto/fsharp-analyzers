module M

open System.Security.Cryptography

let rng = lazy (RNGCryptoServiceProvider.Create())
