open Extended;

let part1 = input => {
  let flush = (group, answers) => group |> List.append([answers]);
  input
  |> List.fold_left(
       ((group, answers), line) =>
         switch (line) {
         | "" => (answers |> flush(group), "")
         | str => (group, answers ++ line)
         },
       ([], ""),
     )
  |> Pair.map(flush)
  |> List.map(String.split_chars)
  |> List.map(List.sort_uniq(Char.compare))
  |> List.map(List.length)
  |> List.fold_left((+), 0)
  |> string_of_int;
};

let part2 = input => {
  let flush = (group, people) => group |> List.append([people]);
  let len = Char.code('z') - Char.code('a');
  let filter_all = 1 lsl (len + 1) - 1;
  input
  |> List.fold_left(
       ((group, people), line) =>
         switch (line) {
         | "" => (people |> flush(group), [])
         | str => (group, line |> flush(people))
         },
       ([], []),
     )
  |> Pair.map(flush)
  |> List.map(
       List.map(people =>
         people
         |> String.split_chars
         |> List.map(Char.code)
         |> List.map(x => x - Char.code('a'))
         |> List.map(i => 1 lsl i)
         |> List.fold_left(Int.logor, 0)
       ),
     )
  |> List.map(List.fold_left(Int.logand, filter_all))
  |> List.map(Int.count_positive_bits(len))
  |> List.fold_left((+), 0)
  |> string_of_int;
};
