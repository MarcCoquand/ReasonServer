open Jest;
open Router;

type identity =
  | Arrived;

type optionals =
  | ArrivedWithOptional(option(string));

type multiOpts =
  | ArrivedWithOptionals(option(string), option(string));
type containsInt =
  | ArrivedWithInt(int);
type containsText =
  | ArrivedWithString(string);
type branch =
  | First
  | Second;
type idRouter = Router.t(identity => identity, identity);
type branchRouter = Router.t(branch => branch, branch);
type hasIntRouter = Router.t(containsInt => containsInt, containsInt);
type hasStringRouter = Router.t(containsText => containsText, containsText);
type optionalRouter = Router.t(optionals => optionals, optionals);
type queriesRouter = Router.t(multiOpts => multiOpts, multiOpts);
let primitiveParse = (str, router) => parseUrl(router, HttpMethod.GET, str);
let isRouter: idRouter = is("hello") ==> Arrived;
let topRouter: idRouter = top ==> Arrived;
let intRouter: hasIntRouter = int ==> (i => ArrivedWithInt(i));
let textRouter: hasStringRouter = text ==> (s => ArrivedWithString(s));
let slashRouter: idRouter = is("hello") >- is("world") ==> Arrived;
let oneOfRouter: branchRouter =
  oneOf([is("hello") ==> First, is("world") ==> Second]);
let getRouter: idRouter = is("world") |> get(Arrived);
let queryRouter: optionalRouter =
  query("hello", text) ==> (s => ArrivedWithOptional(s));
let queriesRouter: queriesRouter =
  query("hello", text)
  >- query("world", text)
  ==> ((s1, s2) => ArrivedWithOptionals(s1, s2));

describe("Chompers chomp", () => {
  Expect.(
    test("segment chomps up until stop", () => {
      let url = "hello&end";
      let (offset, nextOffset, length) =
        chompSegment(url, 0, String.length(url));
      let subString = String.sub(url, 0, offset);

      expect(subString) |> toBe("hello");
    })
  );
  Expect.(
    test("query chomps first arg", () => {
      let url = "hello=hi&world=tf";
      let chomped =
        chompQueries(url, 0, String.length(url), Belt.Map.String.empty);
      expect(Belt.Map.String.get(chomped, "hello")) |> toBe(Some("hi"));
    })
  );
  Expect.(
    test("query chomps second arg", () => {
      let url = "hello=hi&world=tf";
      let chomped =
        chompQueries(url, 0, String.length(url), Belt.Map.String.empty);
      expect(Belt.Map.String.get(chomped, "world")) |> toBe(Some("tf"));
    })
  );
});
describe("Parses primitives", () => {
  Expect.(
    test("is", () =>
      expect(primitiveParse("/hello", isRouter)) |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("top", () =>
      expect(primitiveParse("/", topRouter)) |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("text", () =>
      expect(primitiveParse("/hello", textRouter))
      |> toEqual(Some(ArrivedWithString("hello")))
    )
  );

  Expect.(
    test("slash", () =>
      expect(primitiveParse("/hello/world", slashRouter))
      |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("int", () =>
      expect(primitiveParse("/5", intRouter))
      |> toEqual(Some(ArrivedWithInt(5)))
    )
  );
  Expect.(
    test("oneOf first branch", () =>
      expect(primitiveParse("/hello", oneOfRouter)) |> toBe(Some(First))
    )
  );
  Expect.(
    test("oneOf second branch", () =>
      expect(primitiveParse("/world", oneOfRouter)) |> toBe(Some(Second))
    )
  );

  Expect.(
    test("httpmethods", () =>
      expect(primitiveParse("/world", getRouter)) |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("query", () =>
      expect(primitiveParse("?hello=damn", queryRouter))
      |> toEqual(Some(ArrivedWithOptional(Some("damn"))))
    )
  );
  Expect.(
    test("queries", () =>
      expect(primitiveParse("?hello=hi&world=tf", queriesRouter))
      |> toEqual(Some(ArrivedWithOptionals(Some("hi"), Some("tf"))))
    )
  );
  Expect.(
    test("queries reverse order", () =>
      expect(primitiveParse("?world=tf&hello=hi", queriesRouter))
      |> toEqual(Some(ArrivedWithOptionals(Some("hi"), Some("tf"))))
    )
  );
});