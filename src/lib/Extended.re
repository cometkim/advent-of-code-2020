module Bool = {
  include Bool;

  let xor = (x, y) =>
    switch (x, y) {
    | (true, true) => false
    | (true, false) => true
    | (false, true) => true
    | (false, false) => false
    };

  let (<<>>) = xor;
};

module String = {
  include String;

  let rec join = (sep, strings) =>
    switch (strings) {
    | [head] => head
    | [head, ...tail] => head ++ sep ++ join(sep, tail)
    | [] => ""
    };

  let get_opt = (index, str) => {
    switch (str.[index]) {
    | ch => Some(ch)
    | exception (Invalid_argument(_)) => None
    };
  };
};

module Option = {
  include Option;

  let filter = (f, o) => {
    o |> map(v => f(v) ? Some(v) : None) |> Option.join;
  };
};

module Int = {
  include Int;

  let clamp = ((min, max), v) =>
    switch (v) {
    | v when v < min => min
    | v when v > max => max
    | _ => v
    };
};
