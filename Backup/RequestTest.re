open Jest;
open Request;

type arrivedTwo =
  | ArrivedWithOptionals(option(string), option(string));
describe("Parse query parameter", () => {
  Expect.(
    test("parses first arg", () => {
      let url = "hello=hi&world=tf";
      let chomped =
        queries(url, 0, String.length(url), Belt.Map.String.empty);
      expect(Belt.Map.String.get(chomped, "hello")) |> toEqual(Some("hi"));
    })
  );
  Expect.(
    test("parses chomps second arg", () => {
      let url = "hello=hi&world=tf";
      let chomped =
        queries(url, 0, String.length(url), Belt.Map.String.empty);
      expect(Belt.Map.String.get(chomped, "world")) |> toEqual(Some("tf"));
    })
  );

  let simpleQuery =
    mockGet("?hello=damn") |> query("hello", Optional.string);

  let handleTwo = (s: option(string), s2: option(string)): arrivedTwo =>
    ArrivedWithOptionals(s, s2);

  let twoQuery =
    mockGet("?hello=hi&world=bye")
    |> map(_ => handleTwo)
    |> query("hello", Optional.string);

  Expect.(
    test("query", () =>
      expect(
        mockGet("?hello=damn")
        |> query("hello", Optional.string)
        |> (req => req.arguments),
      )
      |> toEqual(Some("damn"))
    )
  );

  Expect.(
    test("queries", () =>
      expect(twoQuery |> (req => req.arguments))
      |> toEqual(ArrivedWithOptionals(Some("hi"), Some("tf")))
    )
  );
  let twoQueryReverse =
    mockGet("?world=tf&hello=hi")
    |> map(_ => handleTwo)
    |> query("hello", Optional.string)
    |> query("world", Optional.string);
  Expect.(
    test("queries reverse order", () =>
      expect(twoQueryReverse |> (req => req.arguments))
      |> toEqual(ArrivedWithOptionals(Some("hi"), Some("tf")))
    )
  );
});