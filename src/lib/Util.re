let rec joinStrings = (sep, strings) =>
  switch (strings) {
  | [head] => head
  | [head, ...tail] => head ++ sep ++ joinStrings(sep, tail)
  | [] => ""
  };
