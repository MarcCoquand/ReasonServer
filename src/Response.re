type content('a) = {
  code: Status.code,
  headers: Header.Map.t(string),
  contentType: MediaType.t,
  body: 'a,
  encoding: Encoding.t,
};
type modifier =
  | Code(Status.code)
  | Headers(Header.Map.t(string))
  | Encoding(Encoding.t)
  | ContentType(MediaType.t);
type t('a) = 'a => content(string);
let compose = (f, g, x) => f(g(x));
type nobody =
  | NoBody;

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

let contramap: type a b. (a => b, t(b)) => t(a) =
  (cf, response) => compose(response, cf);

let contraError = (error, res) => contramap(_ => error, res);

type handler('b, 'a) = 'b => Result.t('a);

let contramapH: type a b c. (a => b, handler(b, c)) => handler(a, c) =
  (cf, h) => compose(h, cf);

let encode: type a. (a, t(a)) => content(string) =
  (value, response) => response(value);

// let encodeResult =
//     (result: Result.t('a), response: t('a)): Result.t(content(string)) =>
//   switch (result) {
//   | Ok(value) => Result.Ok(encode(value, response))
//   | Failed(m, c, t) => Result.Failed(m, c, t)
//   };
let id: type a. a => a = x => x;
let set = (modifier, response) =>
  switch (modifier) {
  | Code(code) => {...response, code}
  | Encoding(encoding) => {...response, encoding}
  | ContentType(contentType) => {...response, contentType}
  | Headers(headers) => {...response, headers}
  };
let lift: t(content(string)) = id;
let default: t(string) =
  x => {
    code: Status.Ok200,
    headers: Header.Map.empty,
    contentType: MediaType.Plain,
    body: x,
    encoding: Encoding.Ascii,
  };

// Contramap the response
// let contramap: type a b. (a => b, t(b)) => t(a) =
//   (cf, response) => {...response, body: compose(response.body, cf)};

let modifyField = (cf: 'a => ('b, modifier), fb: t('b)): t('a) =>
  a => {
    let (body, modification) = cf(a);
    let response = fb(body);

    set(modification, response);
  };

let setCode = (code: Status.code, response: t('a)): t('a) =>
  modifyField(r => (r, Code(code)), response);

let setContentType = (mediaType, response: t('a)): t('a) =>
  modifyField(res => (res, ContentType(mediaType)), response);

let resultToEither = res =>
  switch (res) {
  | Result.Ok(a) => Result.Right(a)
  | Result.Failed(m, c, t) =>
    Result.Left(error(~message=m, ~code=c, ~method=t))
  };

let choose: type a b c. (a => Result.either(b, c), t(b), t(c)) => t(a) =
  (chooser, resp, encoder2, a) => {
    let res = chooser(a);

    switch (res) {
    | Right(a) => resp(a)
    | Left(err) => encoder2(err)
    };
  };

let divide:
  type a b c. (a => (Result.t(b), Result.t(c)), t(b), t(c)) => t(a) =
  (f, fb, fc, a) => {
    let (resA, resB) = f(a);

    switch (resA) {
    | Ok(success) => fb(success)
    | Failed(_, _, _) =>
      switch (resB) {
      | Ok(success) => fc(success)
      | Failed(m, c, t) => error(~message=m, ~code=c, ~method=t)
      }
    };
  };
let attempt: type a b. t(a) => t(Result.t(a)) =
  response => choose(resultToEither, response, err => err);

let contraApply: type a b. (Result.t(a => b), t(b)) => t(a) =
  (r, tb, a) => {
    switch (r) {
    | Ok(f) => tb(f(a))
    | Failed(m, c, t) => error(~message=m, ~code=c, ~method=t)
    };
  };

let fromResult: type a b. Result.t(content(string)) => content(string) =
  res =>
    switch (res) {
    | Ok(a) => a
    | Failed(m, c, t) => error(~message=m, ~code=c, ~method=t)
    };

let applyHandler = (handlerResult, code, res) =>
  modifyField(a => (a, Code(code)), res) |> choose(handlerResult);

let encode: type a. (a, t(a)) => content(string) =
  (value, response) => response(value);

let json = (encoder: 'a => Js.Json.t, response: t(string)): t('a) => {
  response
  |> contramap(Json.stringify)
  |> contramap(encoder)
  |> setContentType(MediaType.Json);
};