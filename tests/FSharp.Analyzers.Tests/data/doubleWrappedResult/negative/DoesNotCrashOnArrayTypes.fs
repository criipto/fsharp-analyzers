module M

let main (input : string array) : string =
  if input.Length > 0 then
    input[0]
  else
    "none"