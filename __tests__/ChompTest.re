open Jest;
open Chomp;

describe("Chompers chomp", () =>
  Expect.(
    test("segment chomps up until stop", () => {
      let url = "hello&end";
      let (offset, nextOffset, length) =
        Chomp.segment(url, 0, String.length(url));
      let subString = String.sub(url, 0, offset);

      expect(subString) |> toEqual("hello");
    })
  )
);