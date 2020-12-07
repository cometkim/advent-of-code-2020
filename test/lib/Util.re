open Lib;
open TestFramework;

describe("Util", ({test, _}) => {
  test("String.join", ({expect}) => {
    let input = ["a", "b", "c"];
    expect.string(input |> Util.String.join(",")).toEqual("a,b,c");
  })
});
