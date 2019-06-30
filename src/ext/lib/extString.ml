include String

let split_2_on_char char string =
  let i = index string char in
  sub string 0 i, sub string (i + 1) (length string - i - 1)

let endswith needle haystack =
  let ln = length needle in
  let lh = length haystack in
  lh >= ln && needle = sub haystack (lh - ln) ln
