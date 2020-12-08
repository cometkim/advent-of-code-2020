open Lib;
open TestFramework;

describe("Day5", ({test}) => {
  test("part1", ({expect}) => {
    let cases = [
      ("FBFBBFFRLR", "357"),
      ("BFFFBBFRRR", "567"),
      ("FFFBBBFRRR", "119"),
      ("BBFFBBFRLL", "820"),
    ];

    cases
    |> List.iter(case => {
         let (line, expected) = case;
         expect.string(Day5.part1([line])).toEqual(expected);
       });

    let input =
      cases
      |> List.map(case => {
           let (line, _) = case;
           line;
         });

    expect.string(Day5.part1(input)).toEqual("820");
  })
});
