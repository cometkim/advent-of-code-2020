open Extended;

type length = [ | `cm(int) | `in_(int)];

type eye_color = [ | `amb | `blu | `brn | `gry | `grn | `hzl | `oth];

type passport_input = {
  birth_year: option(int),
  issue_year: option(int),
  expiration_year: option(int),
  height: option(length),
  hair_color: option(string),
  eye_color: option(eye_color),
  passport_id: option(string),
  country_id: option(string),
};

type passport =
  | Invalid
  | Valid({
      birth_year: int,
      issue_year: int,
      expiration_year: int,
      height: length,
      hair_color: string, // Hair color in passport???
      eye_color, // Eye color???
      passport_id: string,
      country_id: option(string),
    });

module Parser = {
  open Angstrom;
  include Util.Parser;

  let any_value =
    take_while1(t => Token.is_letter(t) || Token.is_digit(t) || t == '#');
  let pair = (k, v) => lift2((_k, v) => v, k <* char(':'), v);

  module Key = {
    let birth_year = string("byr");
    let issue_year = string("iyr");
    let expiration_year = string("eyr");
    let height = string("hgt");
    let hair_color = string("hcl");
    let eye_color = string("ecl");
    let passport_id = string("pid");
    let country_id = string("cid");
  };
  module Value = {
    let year = take_while1(Token.is_digit) >>| int_of_string;
    let length_cm =
      take_while1(Token.is_digit)
      <* string("cm")
      >>| (v => `cm(int_of_string(v)));
    let length_inch =
      take_while1(Token.is_digit)
      <* string("in")
      >>| (v => `in_(int_of_string(v)));
    let length = length_cm <|> length_inch;
    let color = char('#') *> take_while1(Token.is_hex);
    let eye_color =
      string("amb")
      <|> string("blu")
      <|> string("brn")
      <|> string("gry")
      <|> string("grn")
      <|> string("hzl")
      <|> string("oth")
      >>| (
        v =>
          switch (v) {
          | "amb" => `amb
          | "blu" => `blu
          | "brn" => `brn
          | "gry" => `gry
          | "grn" => `grn
          | "hzl" => `hzl
          | "oth" => `oth
          | _ => failwith("invalid color token for eye_color")
          }
      );
  };
  module Property = {
    let birth_year = pair(Key.birth_year, any_value) >>| (v => `byr(v));
    let issue_year = pair(Key.issue_year, any_value) >>| (v => `iyr(v));
    let expiration_year =
      pair(Key.expiration_year, any_value) >>| (v => `eyr(v));
    let height = pair(Key.height, any_value) >>| (v => `hgt(v));
    let hair_color = pair(Key.hair_color, any_value) >>| (v => `hcl(v));
    let eye_color = pair(Key.eye_color, any_value) >>| (v => `ecl(v));
    let passport_id = pair(Key.passport_id, any_value) >>| (v => `pid(v));
    let country_id = pair(Key.country_id, any_value) >>| (v => `cid(v));

    let any =
      birth_year
      <|> issue_year
      <|> expiration_year
      <|> height
      <|> hair_color
      <|> eye_color
      <|> passport_id
      <|> country_id;
  };

  let passport =
    sep_by(space <|> eol, Property.any)
    >>| (
      props => {
        let input =
          props
          |> List.fold_left(
               (input, prop) =>
                 switch (prop) {
                 | `byr(v) =>
                   let birth_year =
                     v
                     |> parse_string(~consume=All, Value.year)
                     |> Result.to_option
                     |> Option.filter(v => 1920 <= v && v <= 2002);
                   {...input, birth_year};
                 | `iyr(v) =>
                   let issue_year =
                     v
                     |> parse_string(~consume=All, Value.year)
                     |> Result.to_option
                     |> Option.filter(v => 2010 <= v && v <= 2020);
                   {...input, issue_year};
                 | `eyr(v) =>
                   let expiration_year =
                     v
                     |> parse_string(~consume=All, Value.year)
                     |> Result.to_option
                     |> Option.filter(v => 2020 <= v && v <= 2030);
                   {...input, expiration_year};
                 | `hgt(v) =>
                   let height =
                     v
                     |> parse_string(~consume=All, Value.length)
                     |> Result.to_option
                     |> Option.filter(height =>
                          switch (height) {
                          | `cm(length) => 150 <= length && length <= 193
                          | `in_(length) => 59 <= length && length <= 76
                          }
                        );
                   {...input, height};
                 | `hcl(v) =>
                   let hair_color =
                     v
                     |> parse_string(~consume=All, Value.color)
                     |> Result.to_option;
                   {...input, hair_color};
                 | `ecl(v) =>
                   let eye_color =
                     v
                     |> parse_string(~consume=All, Value.eye_color)
                     |> Result.to_option;
                   {...input, eye_color};
                 | `pid(v) => {...input, passport_id: Some(v)}
                 | `cid(v) => {...input, country_id: Some(v)}
                 },
               {
                 birth_year: None,
                 issue_year: None,
                 expiration_year: None,
                 height: None,
                 hair_color: None,
                 eye_color: None,
                 passport_id: None,
                 country_id: None,
               },
             );
        switch (input) {
        | {
            birth_year: Some(birth_year),
            issue_year: Some(issue_year),
            expiration_year: Some(expiration_year),
            height: Some(height),
            hair_color: Some(hair_color),
            eye_color: Some(eye_color),
            passport_id: Some(passport_id),
            country_id,
          } =>
          Valid({
            birth_year,
            issue_year,
            expiration_year,
            height,
            hair_color,
            eye_color,
            passport_id,
            country_id,
          })
        | _ => Invalid
        };
      }
    );

  let parse = input =>
    input
    |> String.join("\n")
    |> parse_string(~consume=All, sep_by(eol, passport))
    |> Result.get_ok;
};

let part1 = input => {
  input
  |> Parser.parse
  |> List.filter(passport => passport != Invalid)
  |> List.length
  |> string_of_int;
};

let part2 = input => {
  input
  |> Parser.parse
  |> List.filter(passport =>
       switch (passport) {
       | Valid(passport) => passport.passport_id |> String.length == 9
       | Invalid => false
       }
     )
  |> List.length
  |> string_of_int;
};
