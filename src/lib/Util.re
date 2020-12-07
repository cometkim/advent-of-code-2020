module String = {
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

module Boolean = {
  let xor = (x, y) =>
    switch (x, y) {
    | (true, true) => false
    | (true, false) => true
    | (false, true) => true
    | (false, false) => false
    };

  let (<<>>) = xor;
};
