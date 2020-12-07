let string_of_multiply = l =>
  l
  |> List.map(float_of_int)
  |> List.fold_left(( *. ), 1.0)
  |> string_of_float
  |> String.split_on_char('.')
  |> List.hd;
