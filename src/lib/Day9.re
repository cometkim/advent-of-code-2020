open Extended;

type preamble = (int, list(int));

let part1 = input => {
  let length = 25;

  let rec has_sum = (x, l) => {
    switch (l) {
    | [] => false
    | [n] => false
    | [n, ...other] =>
      switch (other |> List.find_opt(m => n + m == x)) {
      | Some(_) => true
      | None => has_sum(x, other)
      }
    };
  };

  let preamble_of = (x, length) => {
    (x, List.init(length + 1, i => x - length + i));
  };
  let preamble_of = preamble_of(_, length);

  input
  |> List.map(int_of_string)
  |> List.map(preamble_of)
  |> List.fold_left(
       (acc, preamble) => {
         let (result, queue) = acc;
         switch (result) {
         | Some(_) => acc
         | None =>
           switch (queue |> List.length) {
           | l when l >= length =>
             let x = preamble |> fst;
             let candidates =
               queue
               @ [preamble]
               |> List.map(snd)
               |> List.flatten
               |> List.sort_uniq(Int.compare);
             (
               candidates |> has_sum(x) ? None : Some(x),
               (queue |> List.tl) @ [preamble],
             );
           | _ => (None, queue @ [preamble])
           }
         };
       },
       (None, []),
     )
  |> fst
  |> Option.get
  |> string_of_int;
};

let part2 = input => {
  let step1 = input |> part1 |> int_of_string;
  let numbers =
    input |> List.map(int_of_string) |> List.until(n => n == step1);
  let all_slices = (length, l) => {
    let max = l |> List.length;
    List.init(max - length, i => i)
    |> List.map(i => l |> List.slice2(i, length + i));
  };

  let cursor = ref(0);
  let length = numbers |> List.length;
  let break = ref(false);
  let slice = ref(None);
  while (! break^ && cursor^ <= length) {
    cursor := cursor^ + 1;
    slice :=
      numbers
      |> all_slices(length - cursor^)
      |> List.find_opt(slice => {
           let sum = slice |> List.fold_left((+), 0);
           sum == step1;
         });
    if (slice^ != None) {
      break := true;
    };
  };

  let result = slice^ |> Option.get |> List.fast_sort(Int.compare);
  let smallest = result |> List.hd;
  let largest = result |> List.ft;

  smallest + largest |> string_of_int;
};
