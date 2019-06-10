open Result;
//------------------------------------------------------------------------------
// SPEC
//
// Spec creates a pipeline for request to response. If any of the computations
// it will throw the correct error response.
//
// Spec is implemented using profunctors, however it is unclear if these are
// truly needed or just overengineering. It's also questionable if they were
// correctly.
type parsedUri;
type encoded = string;

type endpoint = computation(Request.t, Response.content(encoded));

type t('handlerArguments, 'unencodedResponse) =
  Result.computation(
    Request.t,
    ('handlerArguments, Response.t('unencodedResponse)),
  );
let id: type a. a => a = x => x;

let (|:) = (a, b) => a |> Result.andThen(b);

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
  type a handler unencoded.
    (list((MediaType.t, unencoded => string)), t(handler, string)) =>
    t(handler, unencoded) =
  (contentTypes, builder) => {
    let makeList = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let makeEncoder = (req: Request.t) =>
      Result.attempt(
        ~message="Unsupported Media Type: " ++ MediaType.toString(req.accept),
        ~code=Status.UnsupportedMediaType415,
        ~contenttype=MediaType.Plain,
        Belt.Map.get(makeList(contentTypes)),
        req.accept,
      );
    //           --- (encoder(req))--------
    //          /                           \
    // (req) =>                              => (handler,encoded(response))
    //          \                           /
    //           --- response -------------
    builder
    |> branch(run(makeEncoder))
    |> andThen(
         merge(
           runFailsafe((encoder, (handler, response)) =>
             (handler, Response.contramap(encoder, response))
           ),
         ),
       );
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

let query = (parameter, parser, builder): t('handler, 'response) =>
  lmap(Request.parseQueries, builder)
  |> branch(runFailsafe(Request.query(parameter, parser)))
  |> andThen(
       merge(
         runFailsafe((arg, (handler, response)) =>
           (handler(arg), response)
         ),
       ),
     );

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
    (list((MediaType.t, string => option(a))), t(a => b, c)) => t(b, c) =
  (contentTypes, builder) => {
    let makeMap = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let acceptsMap = makeMap(contentTypes);
    let decodeBody = (req: Request.t) =>
      Result.attempt(
        ~message=
          "Could not parse body with content type: "
          ++ MediaType.toString(req.contentType),
        ~code=Status.BadRequest400,
        ~contenttype=MediaType.Plain,
        Request.decodeBody(acceptsMap),
        req,
      );
    builder
    |> branch(run(decodeBody))
    |> andThen(
         merge(
           runFailsafe((arg, (handler, response)) =>
             (handler(arg), response)
           ),
         ),
       );
  };
let endpoint = (~handler): t('a, 'b) =>
  runFailsafe((request: Request.t) => (handler, Response.lift));

let success = (~code=Status.Ok200, builder) =>
  //                --- extract result ----
  //               /                       \
  // setHandler =>                           => Response.encode => successCode
  //               \                       /
  //                --- response ---------
  builder
  |> andThen(merge(runFailsafe(Response.encodeResult)))
  |> andThen(run(id))
  |> rmap(Response.setCode(code));