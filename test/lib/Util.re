open Lib;
open TestFramework;

describe("Util", ({test, _}) => {
  test("joinStrings", ({expect}) => {
    let input = ["a", "b", "c"];
    expect.string(input |> Util.joinStrings(",")).toEqual("a,b,c");
  })
});
