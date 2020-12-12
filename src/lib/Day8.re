open Extended;

type instruction =
  | Accumulator(int)
  | Jump(int)
  | Noop(int);

type result =
  | Corrupted(int, int)
  | Fixed(int);

module Parser = {
  open Angstrom;

  include Util.Parser;

  let accumulator = string("acc");
  let jump = string("jmp");
  let noop = string("nop");
  let operation = accumulator <|> jump <|> noop;

  let sign = char('-') <|> char('+');
  let arg = lift2((sign, num) => sign == '+' ? num : - num, sign, digits);

  let instruction =
    lift2(
      (operation, arg) =>
        switch (operation) {
        | "acc" => Accumulator(arg)
        | "jmp" => Jump(arg)
        | _ => Noop(arg)
        },
      operation <* space,
      lex(arg),
    );

  let parse = lines => {
    lines
    |> List.map(parse_string(~consume=All, instruction))
    |> List.mapi((i, result) =>
         switch (result) {
         | Ok(result) => result
         | Error(message) =>
           failwith("error at line " ++ string_of_int(i + 1) ++ message)
         }
       );
  };
};

let program = instructions => {
  let length = instructions |> List.length;

  let acc = ref(0);
  let flags = ref([]);
  let prev = ref(0);
  let pointer = ref(0);

  let get = List.nth(instructions);
  let add = (i, n) => {
    flags := flags^ @ [i];
    acc := acc^ + n;
  };
  let move = n => {
    prev := pointer^;
    pointer := pointer^ + n;
  };

  let break = ref(false);
  while (! break^ && pointer^ < length) {
    switch (get(pointer^)) {
    | Accumulator(arg) =>
      // break on re-visiting same line of acc
      if (flags^ |> List.mem(pointer^)) {
        break := true;
      } else {
        add(pointer^, arg);
        move(1);
      }
    | Jump(arg) => move(arg)
    | Noop(_) => move(1)
    };
    // break on infinity loop
    if (prev^ == pointer^) {
      break := true;
    };
  };

  break^ ? Corrupted(prev^, acc^) : Fixed(acc^);
};

let part1 = input => {
  let instructions = input |> Parser.parse;
  let Corrupted(_, result) | Fixed(result) = instructions |> program;
  result |> string_of_int;
};

let part2 = input => {
  let instructions = input |> Parser.parse;
  let length = instructions |> List.length;

  let index = ref(0);
  let break = ref(false);
  let result = ref(0);

  let swap = instruction =>
    switch (instruction) {
    | Jump(arg) => Noop(arg)
    | Noop(arg) => Jump(arg)
    | v => v
    };

  while (! break^ && index^ < length) {
    switch (List.nth(instructions, index^)) {
    | Accumulator(arg) => ()
    | v =>
      let instructions = instructions |> List.replace(index^, swap(v));
      switch (instructions |> program) {
      | Corrupted(_, _) => ()
      | Fixed(value) =>
        result := value;
        break := true;
      };
    };
    index := index^ + 1;
  };

  result^ |> string_of_int;
};
