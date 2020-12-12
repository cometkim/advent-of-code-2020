open Lib;
open TestFramework;

describe("day8", ({test}) => {
  let input = [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6",
  ];

  test("part1", ({expect}) => {
    expect.string(Day8.part1(input)).toEqual("5")
  });

  test("part2", ({expect}) => {
    expect.string(Day8.part2(input)).toEqual("8")
  });
});
