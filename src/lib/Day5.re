open Extended;

let rec div = (min, max, l) => {
  switch (l) {
  | [head, ...tail] =>
    let half = (max - min) / 2 + min;
    switch (head) {
    | `lower => tail |> div(min, half)
    | `upper => tail |> div(half + 1, max)
    };
  | [] => min
  };
};

let div_on = (from, len, map_ch, source) => {
  String.sub(source, from, len)
  |> String.to_seq
  |> Seq.map(map_ch)
  |> List.of_seq
  |> div(0, 1 lsl len - 1);
};

let seat_from_id = id => {
  let row =
    id
    |> div_on(
         0,
         7,
         fun
         | 'F' => `lower
         | 'B' => `upper
         | _ => failwith("invalid input"),
       );

  let col =
    id
    |> div_on(
         7,
         3,
         fun
         | 'L' => `lower
         | 'R' => `upper
         | _ => failwith("invalid input"),
       );

  row * 8 + col;
};

let part1 = input => {
  input
  |> List.map(seat_from_id)
  |> List.fast_sort(Int.compare)
  |> List.rev
  |> List.hd
  |> string_of_int;
};

let part2 = input => {
  let seats = input |> List.map(seat_from_id) |> List.sort_uniq(Int.compare);
  seats
  |> List.tl
  |> List.fold_left(
       (prev, seat) => {prev + 1 == seat ? seat : prev},
       seats |> List.hd,
     )
  |> Int.add(1)
  |> string_of_int;
};
