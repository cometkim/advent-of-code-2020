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

  let split_chars = s => s |> String.to_seq |> List.of_seq;
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

  // count_positive_bits len i returns count of positive bits from len length bit fields
  let count_positive_bits = (len, i) => {
    let rec count_bits = (count, len, i) =>
      switch (len) {
      | 0 =>
        let bit = Int.logand(1, i);
        count + (bit == 1 ? 1 : 0);
      | len =>
        let bit = Int.logand(1, i lsr len);
        let count = count + (bit == 1 ? 1 : 0);
        i |> count_bits(count, len - 1);
      };
    i |> count_bits(0, len);
  };
};

module Pair = {
  let first = ((a, b)) => a;
  let second = ((a, b)) => b;

  let map = (f, (a, b)) => f(a, b);
};

module List = {
  include List;

  let tap = (f, l) => {
    l |> iter(f);
    l;
  };

  let tapi = (f, l) => {
    l |> iteri(f);
    l;
  };

  let partitioni = (f, l) => {
    let i = ref(0);
    l
    |> partition(x => {
         let result = f(i^, x);
         i := i^ + 1;
         result;
       });
  };

  let replace = (i, v, l) => {
    let (head, tail) = l |> partitioni((index, _) => index < i);
    let tail =
      switch (tail) {
      | [_, ...rest] => [v, ...rest]
      | [] => raise(Failure("No element at " ++ string_of_int(i)))
      };
    head @ tail;
  };

  let init = (length, f) => Array.init(length, f) |> Array.to_list;

  let until = (f, l) => {
    let copy = ref([]);
    l
    |> List.find_opt(x => {
         copy := copy^ @ [x];
         f(x);
       })
    |> ignore;
    copy^;
  };

  let slice = (n, l) => {
    let length = l |> List.length;
    let copy = ref([]);
    l
    |> iteri((i, x) => {
         switch (n >= 0, length - Int.abs(n)) {
         | (true, n) when i <= n => copy := copy^ @ [x]
         | (false, n) when i >= n => copy := copy^ @ [x]
         | _ => ()
         }
       });
    copy^;
  };

  let slice2 = (begin_, end_, l) => {
    let copy = ref([]);
    l
    |> iteri((i, x) => {
         switch (begin_ <= i, i < end_) {
         | (true, true) => copy := copy^ @ [x]
         | _ => ()
         }
       });
    copy^;
  };

  let rec ft = l =>
    switch (l) {
    | [] => failwith("No elements")
    | [ft] => ft
    | [_, ...tl] => ft(tl)
    };
};
