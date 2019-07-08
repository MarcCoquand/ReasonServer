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
type encoded = string;

/**
* A spec is composed of parts. The parameters are the arguments of the handler
* and the response type (unencoded).
* A response of type 'a means that this response is able to encode something of
* type 'a
* a handler of type Result.t('a) means that the handler will attempt to create
* something of type 'a.
* If part(Result.t('a), 'a) then the function success can be used to create a
* spec.
*/
type endpoint('handlerArguments, 'unencodedResponse) = (
  'handlerArguments,
  Response.t('unencodedResponse),
  Request.t,
);

let cmap = (cf: 'a => 'b, (arg, res, req): endpoint('c, 'b)) => (
  arg,
  Response.contramap(cf, res),
  req,
);

let apply = ((arg, res, req): endpoint('a => 'b, 'c), a): endpoint('b, 'c) => (
  arg(a),
  res,
  req,
);

let setReq =
    (reqChanger, (arg, res, req): endpoint('a, 'b)): endpoint('a, 'b) => (
  arg,
  res,
  reqChanger(req),
);

let complete = ((arg, res, req): endpoint(Result.t('a), Result.t('a))) =>
  res(arg);

type complete('content) = endpoint(Result.t('content), Result.t('content));
let id: type a. a => a = x => x;

/**
* A spec is a function that takes a request and returns an encoded response. The
* best way to create these with automatic error handling is by composing parts
* together and merge them using the success function.
*/
type t = Result.computation(Request.t, Response.content(encoded));
type router = Uri.t(t => t, t);
module Accept = {
  type decoder('a) = string => option('a);
  /**
  * Accept a request body of type application/json by providing a way to decode
  * it.
  */
  let json = (decoder: Json.Decode.decoder('a)): (MediaType.t, decoder('a)) => (
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
  type encoder('a) = 'a => string;
  /**
  * Serialize the body to a text and set content type to text/plain.
  */
  let plain = (encoder: encoder('a)) => (MediaType.Plain, a => encoder(a));

  /**
  * Serialize the response body to application/json and set the content type to
  * applicaiton/json.
  */
  let json = (jsonEncoder: 'a => Js.Json.t): (MediaType.t, encoder('a)) => (
    MediaType.Json,
    a => a |> jsonEncoder |> Json.stringify,
  );
};

/**
* Reads accept header of request and sets the encoder to the matching content
* type. If content type is not found response is set to Unsupported Media Type
* with status code 415.
*/
let accept:
  type a handler unencoded.
    (list((MediaType.t, unencoded => string)), endpoint(handler, string)) =>
    Result.t(endpoint(handler, unencoded)) =
  (contentTypes, builder) => {
    let makeList = x =>
      Belt.Map.fromArray(
        Array.of_list(x),
        ~id=(module Request.MediaComparer),
      );
    let (h, res, req) = builder;
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

    let maybeEncoder = makeEncoder(req);
    Result.map(
      encoder =>
        (
          h,
          Response.contramap(encoder, res)
          |> Response.setContentType(req.accept),
          req,
        ),
      maybeEncoder,
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
    (
      parameter,
      parser,
      builder: endpoint(option('a) => 'handler, 'response),
    )
    : endpoint('handler, 'response) => {
  let (h, res, req) = setReq(Request.parseQueries, builder);

  (h(Request.query(parameter, parser, req)), res, req);
};

// let router = f =>
//   split
//   >=> first(Request.route)
//   >=> merge(runFailsafe(((url, query), req) => {...req, url, query}))
//   >=> branch(req => f(req.url), pure)
//   >=> merge((handler, req) => handler(req));

/**
 * Takes a list of content types to decode the body of the request.
 * The decoded body is then feeded as an argument into the handler.
 */
let contentType:
  type a b c.
    (list((MediaType.t, string => option(a))), endpoint(a => b, c)) =>
    Result.t(endpoint(b, c)) =
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
    let (h, res, req) = builder;
    decodeBody(req) |> Result.map(body => (h(body), res, req));
  };

/**
* Start building an endpoint with the given handler.
*
* The handler must be a function that returns a Result.t('a) otherwise a
* complete spec can not be made!
*/
// let endpoint = (~handler, request: Request.t): endpoint('a, 'b) =>
//   (request: Request.t) => Ok((handler, Response.lift, request));

/**
* Sets the success code and encodes the response.
* Default success code is Ok 200.
*/
let success = (~code=Status.Ok200, builder) =>
  // --- extract result ---
  //                        \
  //                         => Response.encode => successCode
  //                        /
  // --- response ---------
  builder |> rmap(((h, res)) => (h, Response.setCode(code, res)));

let withParams = (spec, uri) =>
  Uri.map(
    args => spec |> rmap(((handler, res)) => (handler(args), res)),
    uri,
  );

module Router = {
  type t('a, 'b) =
    | Top: t(('a, 'a), ('a, 'a))
    | Exact(string): t(('a, 'b), ('a, 'b))
    | Custom(string => option('a)): t(('a => 'b, 'c), ('b, 'c))
    | Slash(t(('a, 'b), ('c, 'd)), t(('c, 'd), ('e, 'f)))
      : t(('a, 'b), ('e, 'f))
    | Integer: t((int => 'a, 'b), ('a, 'b))
    | Method(HttpMethod.t): t(('a, 'b), ('a, 'b))
    | Accept(list((MediaType.t, 'b => string)))
      : t(('a, string), ('a, 'b))
    | ContentType(list((MediaType.t, string => option('a))))
      : t(('a => 'b, 'c), ('b, 'c))
    | Map(Status.code, 'a, t(('a, string), (Result.t('c), 'c)))
      : t(('b, string) => string, string)
    | Alternative(
        t(('a, string) => string, string),
        t(('a, string) => string, string),
      )
      : t(('a, string) => string, string);
  // | Map(Status.code, 'a, t(('a, 'l), ('b, 'd)))
  //   : t(('b => 'c, 'l), ('c, 'd));

  let id: type x. x => x = x => x;

  let concatMap = (f, l) => List.map(f, l) |> List.concat;
  //------------------------------------------------------------------------------
  // HELPERS
  let mapHelp = (f, state) =>
    (f(first(state)), second(state)) |> (value => value);

  let makeResponse:
    type a.
      (Status.code, endpoint(Result.t(a), a)) => Response.content(string) =
    (code, (h, res, _)) =>
      Response.setCode(code, res)
      |> Response.attempt
      |> (result => result(h));

  // TODO, extract all functions and make them like the specs

  //------------------------------------------------------------------------------
  // PARSING
  let rec attempt:
    type a b c d.
      (t((a, b), (c, d)), endpoint(a, b)) => Result.t(endpoint(c, d)) =
    (route, state) => {
      let (handler, response, request) = state;
      switch (route) {
      | Top => Ok(state)

      | Exact(str) =>
        parseExact(str, request)
        |> Result.map(newReq => (handler, response, newReq))
      | Integer =>
        parseInt(request)
        |> Result.map(((i, newReq)) => (handler(i), response, newReq))
      | Custom(checker) =>
        parseCustom(checker, request)
        |> Result.map(((v, newReq)) => (handler(v), response, newReq))

      | Slash(before, after) => attempt(before, state) >>= attempt(after)

      | Method(ofType) =>
        requestUsesMethod(ofType, request)
          ? Result.Ok(state) : Result.invalidParse
      | Accept(l) => accept(l, state)
      | ContentType(l) => contentType(l, state)
      //   | Map(code, subValue, subParser) =>
      //     attempt(subParser, (subValue, response, request))
      //     |> Result.map(((v, resPrim, reqPrim)) =>
      //          (handler(v), Response.setCode(code, resPrim), reqPrim)
      //        )
      //   | OneOf(routes) =>
      //     List.map(p => attempt(p, state), routes) |> selectFirstR
      };
    };

  let rec build:
    type a b c.
      (t((a, string) => string, string), endpoint(a, string)) =>
      Result.t(Response.content(string)) =
    (route, (handler, response, request)) => {
      switch (route) {
      | Map(code, subValue, subParser) =>
        attempt(subParser, (subValue, response, request))
        |> Result.map(makeResponse(code))
      | Alternative(a1, a2) =>
        let r1 = build(a1, (handler, response, request));
        let r2 = build(a2, (handler, response, request));
        switch (r1) {
        | Ok(success) => Ok(success)
        | Failed(_, _, _) => r2
        };
      };
    };

  let completeAttempt:
    type a b c.
      (t((a, b), (c, c)), endpoint(a, b)) => Result.t(endpoint(c, c)) =
    (route, state) => attempt(route, state);

  let (<|>) = (expr1, expr2) => Alternative(expr1, expr2);

  //   let s = expr => Singleton(expr);

  let top = Top;
  let is = (str: string) => Exact(str);
  let int = Integer;
  let text = Custom(value => Some(value));

  let custom = (f: string => option('a)) => Custom(f);
  "user";
  let (>-) = (a, b) => Slash(b, a);
  //   let (>->) = (a, b) => Pipe(b, a);
  let handler = (b, code, a) => Map(code, b, a);
  let contenttype = l => ContentType(l);
  let accept = l => Accept(l);

  module Method = {
    let get = Method(HttpMethod.GET);
    let post = Method(HttpMethod.POST);
    let delete = Method(HttpMethod.DELETE);
    let put = Method(HttpMethod.PUT);
    let update = Method(HttpMethod.UPDATE);
    let head = Method(HttpMethod.HEAD);
    let option = Method(HttpMethod.OPTION);
    let connect = Method(HttpMethod.CONNECT);
    let trace = Method(HttpMethod.TRACE);
    let patch = Method(HttpMethod.PATCH);
  };

  let parseHelp:
    type a b.
      (t((a => a, string), (Result.t(b), Result.t(b))), Request.t) =>
      Result.t(endpoint(Result.t(b), Result.t(b))) =
    (router, req) => attempt(router, (id, Response.default, req));

  let parse:
    type a b.
      (t((a => a, string), (Result.t(b), Result.t(b))), Request.t) =>
      Response.content(string) =
    (route, req) => {
      let firstDropped = String.sub(req.url, 1, String.length(req.url) - 1);
      let reqPrim = {...req, url: firstDropped, length: req.length - 1};
      let res = parseHelp(route, reqPrim);
      switch (res) {
      | Ok((a, responseMaker, _)) => responseMaker(a)
      | Failed(m, c, t) => Response.error(~message=m, ~code=c, ~method=t)
      };
    };

  let id: type a. a => a = x => x;

  let primitiveParse:
    type a b.
      (
        t((a => a, Response.content(string)), (Result.t(b), Result.t(b))),
        string
      ) =>
      option((Result.t(b), Response.content(string))) =
    (router, uri) => {
      let firstDropped = String.sub(uri, 1, String.length(uri) - 1);
      Request.mockGet(uri)
      |> (
        (request: Request.t) =>
          attempt(
            router,
            (
              id,
              Response.lift,
              {
                ...request,
                url: firstDropped,
                length: String.length(uri) - 1,
              },
            ),
          )
      )
      |> Result.map(((h, res, _)) => (h, res(h)))
      |> Result.toOption;
    };
};