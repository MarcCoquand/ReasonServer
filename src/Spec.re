//------------------------------------------------------------------------------
// SPEC
//
// Spec creates a pipeline for request to response. If any of the computations
// it will throw the correct error response.
type ongoing;
type complete;
type encoded = string;
type t('a, 'b, 'c) = Result.computation(Request.t('a), 'b);
let id: type a. a => a = x => x;

/**
* Reads accept header of request and sets the encoder to the matching content
* type. If content type is not found response will be error code 415.
*/
let accept =
    (
      contentTypes: Request.contenttype('a),
      builder: t('a, Response.t(encoded), ongoing),
    )
    : t('a, Response.t('b), ongoing) => {
  let makeEncoder = (req: Request.t('a)) =>
    Result.attempt(
      ~message="Unsupported Media Type: " ++ MediaType.toString(req.accept),
      ~code=Status.UnsupportedMediaType415,
      ~contenttype=MediaType.Html,
      Belt.Map.get(contentTypes),
      req.accept,
    );
  //            --- makeEncoder(request) -
  //           /                           \
  // request =>                             => Response.setEncoder
  //           \                           /
  //            --- response -------------
  builder
  |> Result.branch(Result.run(makeEncoder))
  |> Result.andThen(Result.merge(Response.setEncoder));
};

/**
 * Parses a query parameter from URL using a given parser and feeds it as an
 * optional parameter into the handler.
 *
 * // Notice that it requires the parameter published!
 * let books = (published: option(int)): list(book) => ...
 *
 * let api = Spec.query("published", Optional.int) |> Spec.handler(books)
 */

let query = (parameter, parser, builder): t('a, Response.t('b), ongoing) =>
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

let contentType = (~errorContentType=MediaType.Plain, contentTypes, builder) => {
  let decodeBody = (req: Request.t('a => 'a)) =>
    Result.attempt(
      ~message=
        "Could not parse body with content type: "
        ++ MediaType.toString(req.contentType),
      ~code=Status.BadRequest400,
      ~contenttype=errorContentType,
      Request.decodeBody(contentTypes),
      req,
    );
  Result.run(decodeBody) |> Result.andThen(builder);
};

let route =
    (router, builder: t('a, 'b, ongoing))
    : t('a => Cause.Request.t('a), 'b, ongoing) =>
  Result.run(Uri.parse(router)) |> Result.andThen(builder);

let setHandler =
    (
      ~successCode=Status.Ok200,
      handler: 'a => Result.t('b),
      builder: t('a, Response.t('b), ongoing),
    )
    : t('a, Response.content(encoded), complete) =>
  //            --- handle(request)---
  //           /                       \
  // request =>                         => Response.encode
  //           \                       /
  //            --- setStatusCode ----
  builder
  |> Result.branch(
       Result.run((request: Request.t('c)) => handler(request.arguments)),
     )
  |> Result.andThen(
       Result.second(Result.runFailsafe(Response.setCode(successCode))),
     )
  |> Result.andThen(Result.merge(Response.encode));

let run = (builder, request) => Result.evaluate(builder, request);