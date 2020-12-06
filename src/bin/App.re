open Lib;
open Lwt;

exception NoSolution(string);

let (>>=) = Lwt.(>>=);

let days = [
  "day 1 - part 1",
  "day 1 - part 2",
  "day 2",
  "day 3",
  "day 4",
  "day 5",
];

let result =
  Inquire.select("Select a day", ~options=days)
  >>= (
    day =>
      Lwt.return(
        switch (day) {
        | "day 1 - part 1" => Day1.part1()
        | _ => raise(NoSolution(day))
        },
      )
  );

switch (Lwt_main.run(result)) {
| answer => Console.log(answer)
| exception (NoSolution(day)) => Console.log("No Solution for " ++ day)
};
