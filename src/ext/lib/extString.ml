include String

let split_2_on_char char string =
  let i = index string char in
  sub string 0 i, sub string (i + 1) (length string - i - 1)
