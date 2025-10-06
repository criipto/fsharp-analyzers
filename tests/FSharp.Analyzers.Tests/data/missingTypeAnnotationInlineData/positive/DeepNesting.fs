module M

open Xunit

[<Theory>]
[<InlineData(1, "abc")>]
let ``test`` (b : int, ((((d))))) =
  if b = 1 then
    ()
  else
    failwith "error!"
    ()