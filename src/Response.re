type content('a) = {
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
    : content(string) => {
  code,
  headers: Header.Map.empty,
  contentType: method,
  body: message,
  encoding: Encoding.Ascii,
};

type t('a) = content('a => string);

let encode = (unencodedBody, response): content(string) => {
  ...response,
  body: response.body(unencodedBody),
};

// Contramap the response
let setEncoder: type a b. (a => b, t(b)) => t(a) =
  (cf, res) => {...res, body: compose(res.body, cf)};

let setCode = (code: Status.code, res: t('a)): t('a) => {...res, code};

let setContentType = (mediaType, res): t('a) => {
  ...res,
  contentType: mediaType,
};
let json = (encoder: 'a => Js.Json.t, res: t(string)): t('a) =>
  setEncoder(Js.Json.stringify, res)
  |> setEncoder(encoder)
  |> setContentType(MediaType.Json);