open Jest;
open Spec;
open Uri;

let id: type a. a => a = x => x;

type myBody = {hi: string};

let bodyEncoder = myBody => {
  open! Json.Encode;
  Json.Encode.(object_([("hi", string(myBody.hi))]));
};

let bodyDecoder = json => {
  open! Json.Decode;
  Json.Decode.{hi: json |> field("hi", string)};
};

let echoInt = (b: option(int), c: option(int)) =>
  Result.Ok(Belt.Int.toString(Belt.Option.getWithDefault(b, 0)));

let writeHi = Result.Ok("Hello");

let myBodyHandler = (myB: myBody): myBody => id(myB);

let myBodyRoute = builder =>
  builder
  |> accept([Contenttype.json(bodyEncoder)])
  |> contentType(~errorContent=MediaType.Plain, [Accept.json(bodyDecoder)])
  |> handle(Status.Ok200, myBodyHandler);

let writeHi =
  specification
  |> accept([
       Contenttype.plain((s: string) => s),
       Contenttype.json(Json.Encode.string),
     ])
  |> handle(Status.Partial206, writeHi);

let echoIntRoute =
  specification
  |> query("world", Belt.Int.fromString)
  |> query("worl", Belt.Int.fromString)
  |> accept([(MediaType.Plain, (s: string) => s)])
  |> handle(Status.Ok200, echoInt);

describe("Spec", () =>
  Expect.(test("top", () =>
            expect(1) |> toEqual(1)
          ))
);