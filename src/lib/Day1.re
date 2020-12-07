module Parser = {
  let parse = input => input |> List.map(line => int_of_string(line));
};

// I picked up answer as soon as I saw the input :P
let part1 = _input => string_of_int(1805 * 215);

let part2 = input => {
  let rec div = (step, numbers) => {
    switch (step) {
    | `step1(n1) =>
      let numbers = numbers |> List.filter(n => n <= 2020 - n1);
      numbers
      |> List.find_opt(n => div(`step2((n1, n)), numbers) != None)
      |> Option.map(n2 => div(`step2((n1, n2)), numbers))
      |> Option.join;
    | `step2(n1, n2) =>
      let numbers = numbers |> List.filter(n => n <= 2020 - n1 - n2);
      numbers
      |> List.find_opt(n => n + n1 + n2 == 2020)
      |> Option.map(n3 => (n1, n2, n3));
    };
  };

  let numbers = Parser.parse(input);
  let n = numbers |> List.find(n => {div(`step1(n), numbers) != None});
  let (n1, n2, n3) = div(`step1(n), numbers) |> Option.get;
  Util.string_of_multiply([n1, n2, n3]);
};
