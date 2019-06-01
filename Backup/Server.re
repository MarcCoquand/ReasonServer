//------------------------------------------------------------------------------
// RESPONSE
module Response = {
  type t = {
    code: Status.code,
    headers: Header.Map.t(string),
    body: option(string),
    encoding: Encoding.t,
  };

  let fail = (~message=?, code, header): t => {
    code,
    headers: header,
    body: message,
    encoding: Encoding.Utf8,
  };

  let makeUnsafeHeaders = headers =>
    Header.Map.fold((key, value, l) => [(key, value), ...l], headers, [])
    |> Array.of_list;

  let convert = (safeRes: t): Unsafe.Response.abs_t =>
    Unsafe.Response.tToJs({
      bodyString: safeRes.body,
      bodyBuffer: None,
      headers: makeUnsafeHeaders(safeRes.headers),
      statusCode: safeRes.code->Status.toInt,
      encoding: safeRes.encoding->Encoding.toString,
      statusMessage: Status.message(safeRes.code),
    });
};

//------------------------------------------------------------------------------
// RESPONSE BUILDER
module Builder = {
  type t('a) =
    | Constructing(Header.Map.t(string), HttpMethod.t, 'a)
    | Finish(Response.t);

  //----------------------------------------------------------------------------
  // MAPPERS
  let start = (request: Request.t) =>
    Constructing(request.headers, request.method, request.body);
  let failWith = (~message=?, code, header) =>
    Finish(Response.fail(code, header, ~message?));

  let andThen =
      (
        ~failureCode=Status.Error500,
        ~failureMessage=?,
        ~failureContentType="text/plain",
        applyWith: 'a => option('b),
        theValue,
      ) =>
    switch (theValue) {
    | Constructing(headers, method, toApply) =>
      let result = applyWith(toApply);
      switch (result) {
      | Some(value) => Constructing(headers, method, value)
      | None =>
        failWith(
          failureCode,
          Header.Map.singleton("Content-Type", failureContentType),
          ~message=?failureMessage,
        )
      };

    | Finish(response) => Finish(response)
    };

  let map = (mapWith: 'a => 'b, theValue) =>
    switch (theValue) {
    | Constructing(header, method, toMap) =>
      Constructing(header, method, mapWith(toMap))
    | Finish(response) => Finish(response)
    };

  //----------------------------------------------------------------------------
  // HELPERS
  let setHeader = (headerType, value, calc) =>
    switch (calc) {
    | Constructing(h, method, constant) =>
      Constructing(Header.Map.add(headerType, value, h), method, constant)
    | Finish(builder) => Finish(builder)
    };
  let setContentType = (contentType: string, calc: t('a)) =>
    setHeader("Content-Type", contentType, calc);
  let setContentLength = (contentLength, builder) =>
    setHeader("Content-Length", contentLength, builder);
  let send = (success: Status.code, ~encoding=Encoding.Utf8, body): Response.t =>
    switch (body) {
    | Constructing(header, _, a) => {
        code: success,
        headers: header,
        body: a,
        encoding,
      }
    | Finish(failedResponse) => failedResponse
    };

  //----------------------------------------------------------------------------
  // SEND RESPONSE

  /* Stringifies the Json object, sets content type to application/json and
   * returns the response */
  let sendJson = (~code=Status.Ok200, body: t(Js.Json.t)) =>
    body
    |> map(Js.Json.stringify)
    |> map(value => Some(value))
    |> setContentType("application/json")
    |> send(code);

  let sendHtml = (~code=Status.Ok200, body) =>
    body |> setContentType("text/html") |> send(code);

  let sendText = (~code=Status.Ok200, text: string, body) =>
    body
    |> map(_ => Some(text))
    |> setContentType("text/plain")
    |> send(code);
};

//------------------------------------------------------------------------------
// APP
module App = {
  type t = Request.t => Response.t;
  type responseBuilder = Builder.t(string) => Response.t;
  type spec = Spec.t(responseBuilder => responseBuilder, responseBuilder);

  //----------------------------------------------------------------------------
  // CONVERT REQUEST TO NODE REQUEST
  let unsafeHandle = (app: t, nodeReq: Unsafe.Request.abs_t) => {
    let nodeReq = Unsafe.Request.tFromJs(nodeReq);
    let convertedHeaders =
      Array.fold_left(
        (dict, (key, el)) => Header.Map.add(key, el, dict),
        Header.Map.empty,
        Js.Dict.entries(nodeReq.headers),
      );
    let maybeMethod = HttpMethod.fromString(nodeReq.method);
    switch (maybeMethod) {
    | Some(method) =>
      Response.convert(
        app({
          headers: convertedHeaders,
          path: nodeReq.path,
          body: nodeReq.body,
          method,
          isSecure: false,
        }),
      )
    | None =>
      Response.fail(
        Status.BadRequest400,
        convertedHeaders,
        ~message="No method",
      )
      |> Response.convert
    };
  };

  //----------------------------------------------------------------------------
  // MAKE
  let makeApp = (spec: spec, ~notFound="<h1>404 - Not found</h1>"): t =>
    request =>
      Spec.parse(spec, request.method, request.path, request.body)
      |> (
        result =>
          switch (result) {
          | Success(handler) => handler(Builder.start(request))
          | Failed(code, msg) =>
            code == Status.NotFound404
              ? Response.fail(
                  Status.NotFound404,
                  Header.Map.singleton("Content-Type", "application/html"),
                  ~message=notFound,
                )
              : Response.fail(
                  code,
                  Header.Map.singleton("Content-Type", "application/html"),
                  ~message=msg,
                )
          }
      );

  let runRequest = (req: Request.t, spec: spec) => req |> makeApp(spec);
  let start = (~port=3000, server) =>
    Unsafe.server(port, unsafeHandle(server));

  /** start a HTTPS server */
  let startSecure = (~port=3000, ~keyFilepath, ~certificateFilepath, server) =>
    Unsafe.secureServer(
      port,
      keyFilepath,
      certificateFilepath,
      unsafeHandle(server),
    );
};