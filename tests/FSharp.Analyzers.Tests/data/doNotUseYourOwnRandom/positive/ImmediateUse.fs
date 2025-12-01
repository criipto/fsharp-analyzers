module M

open System.Security.Cryptography

let buffer = Array.zeroCreate 32
(new RNGCryptoServiceProvider()).GetNonZeroBytes(buffer)