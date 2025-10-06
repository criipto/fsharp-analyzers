module M

open Xunit

[<Theory>]
[<InlineData(1)>]
let ``test`` (a : int) =
  if a = 1 then
    ()
  else
    failwith "error!"
    ()