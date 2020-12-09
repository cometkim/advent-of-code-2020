open TestFramework;
open Lib.Extended;

describe("String", ({test}) => {
  test("join", ({expect}) => {
    let input = ["a", "b", "c"];
    expect.string(input |> String.join(",")).toEqual("a,b,c");
  })
});

describe("Int", ({test}) => {
  test("count_positive_bits", ({expect}) => {
    expect.int(Int.count_positive_bits(25, 33554433)).toBe(2);
    expect.int(Int.count_positive_bits(1, 1)).toBe(1);
    expect.int(Int.count_positive_bits(25, 1)).toBe(1);
  })
});
