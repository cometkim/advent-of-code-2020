open Lib;
open Lwt.Syntax;

let select_day = () => {
  let* day =
    Inquire.select(
      "Select a day",
      ~options=[
        "day 1 - part 1",
        "day 1 - part 2",
        "day 2 - part 1",
        "day 2 - part 2",
        "day 3 - part 1",
        "day 3 - part 2",
        "day 4",
        "day 5",
      ],
    );
  Lwt.return(day);
};

let read_input = day => {
  let filename =
    switch (day) {
    | "day 1 - part 1"
    | "day 1 - part 2" => Some("day1.txt")
    | "day 2 - part 1"
    | "day 2 - part 2" => Some("day2.txt")
    | "day 3 - part 1"
    | "day 3 - part 2" => Some("day3.txt")
    | _ => None
    };
  switch (filename) {
  | Some(filename) =>
    let* chan = Lwt_io.open_file("./src/bin/input/" ++ filename, ~mode=Input);
    let stream = Lwt_io.read_lines(chan);
    let* lines = stream |> Lwt_stream.to_list;
    Lwt.return(Some(lines));
  | None => Lwt.return(None)
  };
};

let get_answer = (day, input) =>
  switch (day) {
  | "day 1 - part 1" => Some(Day1.part1(input))
  | "day 1 - part 2" => Some(Day1.part2(input))
  | "day 2 - part 1" => Some(Day2.part1(input))
  | "day 2 - part 2" => Some(Day2.part2(input))
  | "day 3 - part 1" => Some(Day3.part1(input))
  | "day 3 - part 2" => Some(Day3.part2(input))
  | _ => None
  };

Lwt_main.run(
  {
    let* day = select_day();
    let* input = read_input(day);
    switch (input) {
    | Some(input) =>
      let answer = get_answer(day, input);
      switch (answer) {
      | Some(answer) => Console.log("The answer is: " ++ answer)
      | None =>
        Console.error("There is no solution for \"" ++ day ++ "\" yet")
      };
    | None => Console.error("No input for \"" ++ day ++ "\"")
    };
    Lwt.return();
  },
);
