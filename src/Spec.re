//------------------------------------------------------------------------------
open Result;
// SPEC
//
// Spec creates a pipeline for request to response. If any of the computations
// it will throw the correct error response.
type parsedUri;
type encoded = string;

type t('a, 'b) = Result.computation('a, 'b);
let id: type a. a => a = x => x;

let compose = (f, g, x) => f(g(x));
module Accept = {
  let json = (decoder: Json.Decode.decoder('a)) => (
    MediaType.Json,
    a =>
      Json.parse(a)
      ->Belt.Option.flatMap(value =>
          try (Some(decoder(value))) {
          | Json.Decode.DecodeError(s) => None
          }
        ),
  );
};
module Contenttype = {
  let plain = (encoder: 'a => string) => (MediaType.Plain, a => encoder(a));
  let json = (encoder: 'a => Js.Json.t) => (
    MediaType.Json,
    a => a |> encoder |> Json.stringify,
  );
};
/**
* Reads accept header of request and sets the encoder to the matching content
* type. If content type is not found response will be error code 415.
*/
let accept:
  type a b.
    (
      list((MediaType.t, b => string)),
      t(Request.t(a), Response.t(string))
    ) =>
    t(Request.t(a), Response.t(b => string)) =
  (contentTypes, builder) => {
    let makeList = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let makeEncoder = (req: Request.t(a)) =>
      Result.attempt(
        ~message="Unsupported Media Type: " ++ MediaType.toString(req.accept),
        ~code=Status.UnsupportedMediaType415,
        ~contenttype=MediaType.Html,
        Belt.Map.get(makeList(contentTypes)),
        req.accept,
      );
    //            --- makeEncoder(request) -
    //           /                           \
    // request =>                             => Response.setEncoder
    //           \                           /
    //            --- response -------------
    builder |> branch(run(makeEncoder)) |> andThen(merge(Response.setBody));
  };

/**
 * Parses a query parameter from URL using a given parser and feeds it as an
 * optional parameter into the handler.
 * <pre><code>
 * // Notice that it requires the parameter published!
 * let books = (published: option(int)): list(book) => ...
 *
 * let api = Spec.query("published", Optional.int) |> Spec.handler(books)
 * </code></pre>
 */

let query = (parameter, parser, builder) =>
  Result.runFailsafe(Request.query(parameter, parser))
  |> Result.andThen(builder);

/**
 * Takes a list of content types to decode the body of the request.
 * The decoded body is then feeded as an argument into the handler.
 *
 * let postBook = (published: option(int)): list(book) => ...
 *
 * let api = Spec.query("published", Optional.int) |> Spec.handler(books)
 */

let contentType:
  type a b c.
    (
      ~errorContent: MediaType.t,
      list((MediaType.t, string => option(a))),
      Result.computation(Request.t(b), Response.t(c))
    ) =>
    Result.computation(Request.t(a => b), Response.t(c)) =
  (~errorContent, contentTypes, builder) => {
    let makeMap = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let acceptsMap = makeMap(contentTypes);
    let decodeBody = (req: Request.t(a => b)) =>
      Result.attempt(
        ~message=
          "Could not parse body with content type: "
          ++ MediaType.toString(req.contentType),
        ~code=Status.BadRequest400,
        ~contenttype=errorContent,
        Request.decodeBody(acceptsMap),
        req,
      );
    Result.run(decodeBody) |> Result.andThen(builder);
  };

let route = (router, builder) =>
  Result.run(Uri.parse(router)) |> Result.andThen(builder);

let setHandler: type a b. (b, Request.t(a)) => Request.t(b) =
  (handler, req) => Request.map(_ => handler, req);

// let simplify =
//     (builder: computation(Request.t(Result.t('a)), Response.t(string)))
//     : computation(Request.t('a), Response.t(string)) =>
//   run((req: Request.t(Result.t('a))) => req.arguments) |> builder;

let start: Request.t(Result.t(string)) => Response.t(string) =
  (s) => (
    switch (s.arguments) {
    | Result.Ok(m) => Response.lift(~code=Status.Ok200, m)
    | Result.Failed(m, c, t) =>
      Response.error(~message=m, ~code=c, ~method=t)
    }:
      Response.t('a)
    //   | Result.Failed(m, c, t) => Response.error(~message=m, ~code=c, ~method=t)
  );

let specification = runFailsafe(start);

let handle = (successCode, handler, builder) =>
  //            --- handle(request)---
  //           /                       \
  // request =>                         => Response.encode
  //           \                       /
  //            --- setSuccessCode ---
  runFailsafe(setHandler(handler))
  |> andThen(builder)
  |> branch(run((req: Request.t(Result.t('a))) => req.arguments))
  |> andThen(merge(Response.encode))
  |> andThen(runFailsafe(Response.setCode(successCode)));