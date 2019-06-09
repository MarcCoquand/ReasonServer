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

type endpoint =
  computation(Request.t(Request.nohandler), Response.content(string));

type t('a, 'b, 'c, 'd) =
  Result.computation(
    (Request.t('a), Response.t('b)),
    (Request.t('c), Response.t('d)),
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
  type handler unencoded.
    list((MediaType.t, unencoded => string)) =>
    t(handler, string, handler, unencoded) =
  contentTypes => {
    let makeList = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let makeEncoder = (req: Request.t(handler)) =>
      Result.attempt(
        ~message="Unsupported Media Type: " ++ MediaType.toString(req.accept),
        ~code=Status.UnsupportedMediaType415,
        ~contenttype=MediaType.Html,
        Belt.Map.get(makeList(contentTypes)),
        req.accept,
      );
    //               --- (encoder(req), req) ---
    //              /                           \
    // (req,res) =>                               => (req,encoded(res))
    //              \                           /
    //               --- response -------------
    first(run(req => makeEncoder(req) >>= (encoder => Ok((encoder, req)))))
    |> andThen(
         merge(
           runFailsafe(((encoder, request), response) =>
             (request, Response.contramap(encoder, response))
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

let query =
    (parameter, parser)
    : t(option('b) => 'handler, 'response, 'handler, 'response) =>
  first(runFailsafe(Request.query(parameter, parser)));

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
      list((MediaType.t, string => option(a)))
    ) =>
    t(a => b, c, b, c) =
  (~errorContent, contentTypes) => {
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
    first(run(decodeBody));
  };

let route: type a b c. Uri.t(a => b, b) => t(a => b, c, b, c) =
  router => first(run(Uri.parse(router)));

let handle:
  type a b. (Status.code, a, t(a, string, Result.t(b), b)) => endpoint =
  (code, handler, builder) =>
    //                --- extract result ----
    //               /                       \
    // setHandler =>                           => Response.encode => successCode
    //               \                       /
    //                --- response ---------
    builder
    |: first(run(Request.extractResult))
    |: merge(runFailsafe(Response.encode))
    |> dimap(
         (request: Request.t(Request.nohandler)) =>
           (Request.pure(handler, request), Response.lift),
         Response.setCode(code),
       );