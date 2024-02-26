func toString*(d: seq[char]): string =
  ## Turns a sequence of char's into a string
  for i in d:
    result.add i

