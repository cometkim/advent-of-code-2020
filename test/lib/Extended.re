open TestFramework;
open Lib.Extended;

describe("Extended.String", ({test, _}) => {
  test("join", ({expect}) => {
    let input = ["a", "b", "c"];
    expect.string(input |> String.join(",")).toEqual("a,b,c");
  })
});
