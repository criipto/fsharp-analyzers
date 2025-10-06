module M

open Xunit

// Should only give one warning

[<Theory>]
[<InlineData("test1")>]
[<InlineData("test2")>]
let ``test``(a) =
  ()