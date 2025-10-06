module M

open Xunit

// Should only give one warning

[<Theory>]
[<InlineData("abc", "{aaa}")>]
[<InlineData("def", "{bbb}")>]
let test(rel : string, placeholder) =
  ()