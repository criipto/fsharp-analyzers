module M

open Xunit

let test = ()

[<Theory>]
[<InlineData(1)>]
let test2 (a : int) =
  if a = 1 then
    ()
  else
    failwith "error"
    ()