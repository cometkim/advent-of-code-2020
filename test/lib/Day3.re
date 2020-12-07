open Lib;
open TestFramework;

describe("Day3", ({test, _}) => {
  let input = [
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#",
  ];

  test("part1", ({expect}) => {
    expect.string(input |> Day3.part1).toEqual("7")
  });

  test("part2", ({expect}) => {
    expect.string(input |> Day3.part2).toEqual("336")
  });
});
