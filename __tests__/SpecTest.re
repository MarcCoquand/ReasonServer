open Jest;
open Spec;

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
type idSpec = Spec.t(identity => identity, identity);
type branchSpec = Spec.t(branch => branch, branch);
type hasIntSpec = Spec.t(containsInt => containsInt, containsInt);
type hasStringSpec = Spec.t(containsText => containsText, containsText);
type optionalSpec = Spec.t(optionals => optionals, optionals);
type queriesSpec = Spec.t(multiOpts => multiOpts, multiOpts);
let primitiveParse = (str, spec) => parse(spec, HttpMethod.GET, str, "");
let isSpec: idSpec = is("hello") ==> Arrived;
let topSpec: idSpec = top ==> Arrived;
let intSpec: hasIntSpec = int ==> (i => ArrivedWithInt(i));
let textSpec: hasStringSpec = text ==> (s => ArrivedWithString(s));
let slashSpec: idSpec = is("hello") >- is("world") ==> Arrived;
let oneOfSpec: branchSpec =
  oneOf([is("hello") ==> First, is("world") ==> Second]);
let getSpec: idSpec = is("world") |> get(Arrived);
let querySpec: optionalSpec =
  query("hello", text) ==> (s => ArrivedWithOptional(s));
let queriesSpec: queriesSpec =
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
    test("top", () =>
      expect(primitiveParse("/", topSpec)) |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("is", () =>
      expect(primitiveParse("/hello", isSpec)) |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("text", () =>
      expect(primitiveParse("/hello", textSpec))
      |> toEqual(Some(ArrivedWithString("hello")))
    )
  );

  Expect.(
    test("slash", () =>
      expect(primitiveParse("/hello/world", slashSpec))
      |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("int", () =>
      expect(primitiveParse("/5", intSpec))
      |> toEqual(Some(ArrivedWithInt(5)))
    )
  );
  Expect.(
    test("oneOf first branch", () =>
      expect(primitiveParse("/hello", oneOfSpec)) |> toBe(Some(First))
    )
  );
  Expect.(
    test("oneOf second branch", () =>
      expect(primitiveParse("/world", oneOfSpec)) |> toBe(Some(Second))
    )
  );

  Expect.(
    test("httpmethods", () =>
      expect(primitiveParse("/world", getSpec)) |> toBe(Some(Arrived))
    )
  );
  Expect.(
    test("query", () =>
      expect(primitiveParse("?hello=damn", querySpec))
      |> toEqual(Some(ArrivedWithOptional(Some("damn"))))
    )
  );
  Expect.(
    test("queries", () =>
      expect(primitiveParse("?hello=hi&world=tf", queriesSpec))
      |> toEqual(Some(ArrivedWithOptionals(Some("hi"), Some("tf"))))
    )
  );
  Expect.(
    test("queries reverse order", () =>
      expect(primitiveParse("?world=tf&hello=hi", queriesSpec))
      |> toEqual(Some(ArrivedWithOptionals(Some("hi"), Some("tf"))))
    )
  );
});