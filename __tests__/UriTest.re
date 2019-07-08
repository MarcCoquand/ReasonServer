open Jest;
open Uri;

type arrived =
  | Arrived;

type branched =
  | First
  | Second;

let id: type a. a => a = x => x;
describe("Parses primitives", () => {
  let topRouter = top ==> Arrived;
  Expect.(
    test("top", () =>
      expect(primitiveParse(topRouter, "/")) |> toEqual(Some(Arrived))
    )
  );

  let isRouter = is("hello") ==> Arrived;
  Expect.(
    test("is", () =>
      expect(primitiveParse(isRouter, "/hello")) |> toEqual(Some(Arrived))
    )
  );

  let textRouter = text ==> id;
  Expect.(
    test("text", () =>
      expect(primitiveParse(textRouter, "/hello"))
      |> toEqual(Some("hello"))
    )
  );

  let slashRouter = is("hello") ^/ is("world") ==> Arrived;
  Expect.(
    test("slash", () =>
      expect(primitiveParse(slashRouter, "/hello/world"))
      |> toEqual(Some(Arrived))
    )
  );

  let intRouter = int ==> id;
  Expect.(
    test("int", () =>
      expect(primitiveParse(intRouter, "/5")) |> toEqual(Some(5))
    )
  );

  let oneOfRouter = oneOf([is("hello") ==> First, is("world") ==> Second]);
  Expect.(
    test("oneOf first branch", () =>
      expect(primitiveParse(oneOfRouter, "/hello")) |> toEqual(Some(First))
    )
  );
  Expect.(
    test("oneOf second branch", () =>
      expect(primitiveParse(oneOfRouter, "/world"))
      |> toEqual(Some(Second))
    )
  );
});