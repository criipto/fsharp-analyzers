module M

open Xunit

[<Theory>]
[<InlineData(1,2)>]
let ``test``(a, b) =
  if a = b then
    ()
  else
    failwith "error!"
    ()