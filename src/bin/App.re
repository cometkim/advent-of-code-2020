open Lib;
open Lwt.Syntax;

let select_day = () => {
  let* day =
    Inquire.select(
      "Select a day",
      ~options=[
        "day 1 - part 1",
        "day 1 - part 2",
        "day 2",
        "day 3",
        "day 4",
        "day 5",
      ],
    );
  Lwt.return(day);
};

let get_answer = day =>
  switch (day) {
  | "day 1 - part 1" => Some(Day1.part1())
  | _ => None
  };

Lwt_main.run(
  {
    let* day = select_day();
    let answer = get_answer(day);
    switch (answer) {
    | Some(answer) =>
      Console.log("The answer is: " ++ string_of_int(answer))
    | None => Console.error("There is no solution for \"" ++ day ++ "\" yet")
    };
    Lwt.return();
  },
);
