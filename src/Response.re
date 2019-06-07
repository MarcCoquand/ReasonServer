type t('a) = {
  code: Status.code,
  headers: Header.Map.t(string),
  contentType: MediaType.t,
  body: 'a,
  encoding: Encoding.t,
};
let compose = (f, g, x) => f(g(x));
type nobody =
  | NoBody;

let error =
    (
      ~message="Internal server error",
      ~code=Status.Error500,
      ~method=MediaType.Html,
    )
    : t(string) => {
  code,
  headers: Header.Map.empty,
  contentType: method,
  body: message,
  encoding: Encoding.Ascii,
};

let map: type a b. (a => b, t(a)) => t(b) =
  (f, response) => {...response, body: f(response.body)};

let id: type a. a => a = x => x;

let lift: t(nobody) = {
  code: Status.Ok200,
  headers: Header.Map.empty,
  contentType: MediaType.Plain,
  body: NoBody,
  encoding: Encoding.Ascii,
};

// Contramap the response
// let contramap: type a b. (a => b, t(b)) => t(a) =
//   (cf, response) => {...response, body: compose(response.body, cf)};

let setCode = (code: Status.code, response: t('a)): t('a) => {
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
  |> map(encoder)
  |> map(Json.stringify)
  |> setContentType(MediaType.Json);
};