type policy = {
  min: int,
  max: int,
  ch: char,
};

type password = string;

module Parser = {
  open Angstrom;

  module Token = {
    let is_digit =
      fun
      | '0' .. '9' => true
      | _ => false
    and is_word =
      fun
      | 'a' .. 'z'
      | 'A' .. 'Z' => true
      | _ => false
    and is_space =
      fun
      | ' '
      | '\t' => true
      | _ => false;
  };

  let digits = take_while1(Token.is_digit) >>| int_of_string;
  let words = take_while1(Token.is_word);
  let spaces = skip_while(Token.is_space);
  let lex = p => p <* spaces;

  let range = lift2((min, max) => (min, max), digits, char('-') *> digits);
  let policy =
    lift2((range, ch) => (range, ch), lex(range), any_char)
    >>| ((((min, max), ch)) => {min, max, ch});
  let policy_and_password =
    lift2(
      (policy, password) => (policy, password),
      lex(policy <* char(':')),
      words,
    );

  let parse = input => {
    let lines = input |> String.split_on_char('\n');
    lines
    |> List.map(line =>
         switch (line |> parse_string(~consume=All, policy_and_password)) {
         | Ok((policy, password)) => (policy, password)
         | Error(message) => failwith(message)
         }
       );
  };
};

let part1 = input => {
  let validate_with = (policy, password) => {
    let count =
      (password |> String.split_on_char(policy.ch) |> List.length) - 1;
    policy.min <= count && count <= policy.max;
  };

  Parser.parse(input)
  |> List.filter(pair => {
       let (policy, password) = pair;
       password |> validate_with(policy);
     })
  |> List.length
  |> string_of_int;
};

let part2 = input => {
  let validate_with = (policy, password) => {
    let {min, max, ch} = policy;
    let ch1 = password |> Util.String.get_opt(min - 1) |> Option.get;
    let ch2 = password |> Util.String.get_opt(max - 1) |> Option.get;
    Util.Boolean.xor(ch == ch1, ch == ch2);
  };

  Parser.parse(input)
  |> List.filter(pair => {
       let (policy, password) = pair;
       password |> validate_with(policy);
     })
  |> List.length
  |> string_of_int;
};
