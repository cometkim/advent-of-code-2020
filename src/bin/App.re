open Lwt.Syntax;
open Lib;

exception NoSolution(string);

let days = [
  "day 1 - part 1",
  "day 1 - part 2",
  "day 2",
  "day 3",
  "day 4",
  "day 5",
];

let select_answer = () => {
  let* day = Inquire.select("Select a day", ~options=days);
  let result =
    switch (day) {
    | "day 1 - part 1" => Day1.part1()
    | _ => raise(NoSolution(day))
    };
  Lwt.return(result);
};

switch (select_answer() |> Lwt_main.run) {
| answer => Console.log(answer)
| exception (NoSolution(day)) => Console.error("No Solution for " ++ day)
};
