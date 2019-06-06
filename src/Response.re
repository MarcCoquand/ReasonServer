type t('a) = {
  code: Status.code,
  headers: Header.Map.t(string),
  contentType: MediaType.t,
  body: 'a,
  encoding: Encoding.t,
};
let compose = (f, g, x) => f(g(x));

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

let id: type a. a => a = x => x;
let map: type a b. (a => b, t(a)) => t(b) =
  (f, content) => {...content, body: f(content.body)};

let lift = (~code, s): t('a) => {
  code,
  headers: Header.Map.empty,
  contentType: MediaType.Plain,
  body: s,
  encoding: Encoding.Ascii,
};

// Contramap the response
let contramap: type a c. (a => c, t(c)) => Result.computation(t(a), t(c)) =
  (cf, res) => Result.runFailsafe(a => {...a, body: cf(a.body)});

let setBody: type a b. (a, t(b)) => t(a) =
  (f, content) => {...content, body: f};

let setCode = (code: Status.code, response: t('a)): t('a) => {
  ...response,
  code,
};

let setContentType = (mediaType, response) =>
  response
  |> Result.andThen(
       Result.runFailsafe(res => {...res, contentType: mediaType}),
     );

let encode = (value: 'a, res: t('a => string)) => {
  ...res,
  body: res.body(value),
};

let setEncoder = (encoder: 'a => string, res) => {...res, body: encoder};

let json = (encoder: 'a => Js.Json.t, res) => {
  Result.runFailsafe(b => {...b, body: encoder(b.body)})
  |> Result.andThen(
       Result.runFailsafe(a => {...a, body: Json.stringify(a.body)}),
     )
  |> Result.andThen(res)
  |> setContentType(MediaType.Json);
};