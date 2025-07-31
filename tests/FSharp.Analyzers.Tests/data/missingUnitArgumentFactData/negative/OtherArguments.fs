module M

open Xunit

// This analyzer should not warn about this.
// If a warning for this is desired, it should be given by another analyzer.

[<Fact>]
let test a =
  if a = 1 then
    ()
  else
    failwith "error"
    ()