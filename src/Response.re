type content('a) = {
  code: Status.code,
  headers: Header.Map.t(string),
  contentType: MediaType.t,
  body: 'a,
  encoding: Encoding.t,
};

type t('a) = content('a => string);
let compose = (f, g, x) => f(g(x));
type nobody =
  | NoBody;

let error =
    (
      ~message="Internal server error",
      ~code=Status.Error500,
      ~method=MediaType.Html,
    )
    : t('a) => {
  code,
  headers: Header.Map.empty,
  contentType: method,
  body: _ => message,
  encoding: Encoding.Ascii,
};

let map: type a b. (a => b, content(a)) => content(b) =
  (f, response) => {...response, body: f(response.body)};

let contramap: type a b. (a => b, t(b)) => t(a) =
  (cf, response) => {...response, body: compose(response.body, cf)};

let encode: type a. (a, t(a)) => content(string) =
  (value, response) => {...response, body: response.body(value)};

let fromResult = (maybeResponse: Result.t(t('a))) =>
  switch (maybeResponse) {
  | Ok(response) => response
  | Failed(m, c, t) => error(~message=m, ~code=c, ~method=MediaType.Html)
  };
let id: type a. a => a = x => x;

let lift = {
  code: Status.Ok200,
  headers: Header.Map.empty,
  contentType: MediaType.Plain,
  body: id,
  encoding: Encoding.Ascii,
};

// Contramap the response
// let contramap: type a b. (a => b, t(b)) => t(a) =
//   (cf, response) => {...response, body: compose(response.body, cf)};

let setCode =
    (code: Status.code, response: content(string)): content(string) => {
  ...response,
  code,
};

let setContentType = (mediaType, response) => {
  ...response,
  contentType: mediaType,
};

// let encode = (value: 'a, res: t('a)) => {...res, body: res.body(value)};

let setEncoder = (encoder: 'a => string, res) => map(encoder, res);

let json = (encoder: 'a => Js.Json.t, response) => {
  response
  |> contramap(Json.stringify)
  |> contramap(encoder)
  |> setContentType(MediaType.Json);
};