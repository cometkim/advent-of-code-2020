open Lib;
open TestFramework;

describe("Day6", ({test}) => {
  test("part1", ({expect}) => {
    let input = [
      "abc",
      "",
      "a",
      "b",
      "c",
      "",
      "ab",
      "ac",
      "",
      "a",
      "a",
      "a",
      "a",
      "",
      "b",
    ];
    expect.string(Day6.part1(input)).toEqual("11");
  })
});
