open Status;

module Header =
  Map.Make({
    type t = string;
    let compare = compare;
  });

type response = {
  code: Status.code,
  headers: Header.t(string),
  body: option(string),
  encoding: Encoding.t,
};

type responseBuilder('a) =
  | Constructing(Header.t(string), 'a)
  | Finish(response);

let addHeader = (headerType, value, calc) =>
  switch (calc) {
  | Constructing(h, constant) =>
    Constructing(Header.add(headerType, value, h), constant)
  | Finish(responseBuilder) => Finish(responseBuilder)
  };

let setContentType = (contentType: string, calc: responseBuilder('a)) =>
  addHeader("Content-Type", contentType, calc);

let failWith =
    (code: Status.Failure.code, header: Header.t(string), ~message=?) =>
  Finish({
    code: Status.fail(code),
    headers: header,
    body: message,
    encoding: Encoding.Utf8,
  });

let fromOption =
    (
      applyWith: 'a => option('b),
      code: Status.Failure.code,
      ~message=?,
      ~headers=?,
      theValue: 'a,
    )
    : responseBuilder('b) =>
  switch (applyWith(theValue)) {
  | Some(success) =>
    Constructing(Belt.Option.getWithDefault(headers, Header.empty), success)
  | None =>
    failWith(code, Header.singleton("Conent-Type", "text/plain"), ~message?)
  };

let andThen =
    (
      applyWith: 'a => option('b),
      failureCode: Status.Failure.code,
      ~message=?,
      theValue,
    )
    : responseBuilder('b) =>
  switch (theValue) {
  | Constructing(headers, toApply) =>
    fromOption(applyWith, failureCode, ~message?, ~headers, toApply)
  | Finish(response) => Finish(response)
  };

let map = (mapWith: 'a => 'b, theValue): responseBuilder('b) =>
  switch (theValue) {
  | Constructing(header, toMap) => Constructing(header, mapWith(toMap))
  | Finish(response) => Finish(response)
  };

let send =
    (
      success: Status.Success.code,
      ~message=?,
      ~encoding=?,
      body: responseBuilder('a),
    ) =>
  switch (body) {
  | Constructing(header, a) => {
      code: Success(success),
      headers: header,
      body: a,
      encoding: Belt.Option.getWithDefault(encoding, Encoding.Utf8),
    }
  | Finish(failedResponse) => failedResponse
  };

let sendJson = (body, ~code=?) => {
  let successCode =
    switch (code) {
    | Some(success) => success
    | None => Status.Success.Status200
    };
  body
  ->map(Js.Json.stringifyAny, _)
  ->setContentType("application/json", _)
  ->send(successCode, _);
};

let sendHtml = (body, ~code=?) => {
  let successCode =
    switch (code) {
    | Some(success) => success
    | None => Success.Status200
    };
  body->setContentType("text/html", _)->send(successCode, _);
};

let sendText = (body: responseBuilder('a), text: string, ~code=?) => {
  let successCode =
    switch (code) {
    | Some(success) => success
    | None => Success.Status200
    };
  body
  ->map(() => Some(text), _)
  ->setContentType("text/plain", _)
  ->send(successCode, _);
};