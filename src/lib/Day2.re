open Extended;

type policy = {
  min: int,
  max: int,
  ch: char,
};

module Parser = {
  open Angstrom;
  include Util.Parser;

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

  let parse = input =>
    input
    |> List.map(line =>
         line
         |> parse_string(~consume=All, policy_and_password)
         |> Result.get_ok
       );
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
    let ch1 = password |> String.get_opt(min - 1) |> Option.get;
    let ch2 = password |> String.get_opt(max - 1) |> Option.get;
    Bool.xor(ch == ch1, ch == ch2);
  };

  Parser.parse(input)
  |> List.filter(pair => {
       let (policy, password) = pair;
       password |> validate_with(policy);
     })
  |> List.length
  |> string_of_int;
};
