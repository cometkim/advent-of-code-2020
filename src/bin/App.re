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
        "day 4 - part 1",
        "day 4 - part 2",
        "day 5 - part 1",
        "day 5 - part 2",
        "day 6 - part 1",
        "day 6 - part 2",
        "day 7 - part 1",
        "day 7 - part 2",
        "day 8 - part 1",
        "day 8 - part 2",
        "day 9 - part 1",
        "day 9 - part 2",
        "day 10",
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
    | "day 4 - part 1"
    | "day 4 - part 2" => Some("day4.txt")
    | "day 5 - part 1"
    | "day 5 - part 2" => Some("day5.txt")
    | "day 6 - part 1"
    | "day 6 - part 2" => Some("day6.txt")
    | "day 7 - part 1"
    | "day 7 - part 2" => Some("day7.txt")
    | "day 8 - part 1"
    | "day 8 - part 2" => Some("day8.txt")
    | "day 9 - part 1"
    | "day 9 - part 2" => Some("day9.txt")
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
  | "day 4 - part 1" => Some(Day4.part1(input))
  | "day 4 - part 2" => Some(Day4.part2(input))
  | "day 5 - part 1" => Some(Day5.part1(input))
  | "day 5 - part 2" => Some(Day5.part2(input))
  | "day 6 - part 1" => Some(Day6.part1(input))
  | "day 6 - part 2" => Some(Day6.part2(input))
  | "day 7 - part 1" => Some(Day7.part1(input))
  | "day 7 - part 2" => Some(Day7.part2(input))
  | "day 8 - part 1" => Some(Day8.part1(input))
  | "day 8 - part 2" => Some(Day8.part2(input))
  | "day 9 - part 1" => Some(Day9.part1(input))
  | "day 9 - part 2" => Some(Day9.part2(input))
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
