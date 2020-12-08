let string_of_multiply = l =>
  l
  |> List.map(float_of_int)
  |> List.fold_left(( *. ), 1.0)
  |> string_of_float
  |> String.split_on_char('.')
  |> List.hd;

module Parser = {
  open Angstrom;

  module Token = {
    let is_letter =
      fun
      | 'a' .. 'z' => true
      | _ => false;
    let is_digit =
      fun
      | '0' .. '9' => true
      | _ => false;
    let is_hex =
      fun
      | '0' .. '9' => true
      | 'a' .. 'f' => true
      | _ => false;
    let is_word =
      fun
      | 'a' .. 'z'
      | 'A' .. 'Z' => true
      | _ => false;
    let is_space =
      fun
      | ' '
      | '\t' => true
      | _ => false;
  };

  let space = char(' ');
  let eol = char('\n');
  let digits = take_while1(Token.is_digit) >>| int_of_string;
  let words = take_while1(Token.is_word);
  let spaces = skip_while(Token.is_space);
  let lex = p => p <* spaces;
};
