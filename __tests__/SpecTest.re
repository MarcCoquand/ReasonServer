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

let myBodyHandler = (myB: myBody): Result.t(myBody) => Result.Ok(id(myB));

// let writeHi = builder =>
//   builder
//   |> accept([
//        Contenttype.plain((s: string) => s),
//        Contenttype.json(Json.Encode.string),
//      ])
//   |> handle(Status.Partial206, writeHi);

let echoIntRoute =
  query("world", Belt.Int.fromString)
  |> Result.andThen(query("worl", Belt.Int.fromString))
  |> handle(Status.Ok200, echoInt);

let myBodyRoute =
  contentType(~errorContent=MediaType.Plain, [Accept.json(bodyDecoder)])
  |> handle(Status.Ok200, myBodyHandler)
  |> accept([Contenttype.json(bodyEncoder)]);

describe("Spec", () =>
  Expect.(test("Body route", () =>

          ))
);