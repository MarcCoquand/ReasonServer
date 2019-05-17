//------------------------------------------------------------------------------
// RESPONSE
module Response = {
  type t = {
    code: Status.code,
    headers: Header.Map.t(string),
    body: option(string),
    encoding: Encoding.t,
  };

  let fail = (code, header, ~message=?): t => {
    code: Status.fail(code),
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
  open Status;
  type t('a) =
    | Constructing(Header.Map.t(string), HttpMethod.t, 'a)
    | Finish(Response.t);

  //----------------------------------------------------------------------------
  // MAPPERS
  let start = (request: Request.t) =>
    Constructing(request.headers, request.method, request.body);
  let failWith = (code, header, ~message=?) =>
    Finish(Response.fail(code, header, ~message?));

  let andThen =
      (
        applyWith: 'a => option('b),
        ~failureCode=Failure.Status500,
        ~failureMessage=?,
        ~failureContentType="text/plain",
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
  let send =
      (success: Status.Success.code, ~encoding=Encoding.Utf8, body)
      : Response.t =>
    switch (body) {
    | Constructing(header, method, a) => {
        code: Success(success),
        headers: header,
        body: a,
        encoding,
      }
    | Finish(failedResponse) => failedResponse
    };

  //----------------------------------------------------------------------------
  // PARSERS
  let parse =
      (
        parser: 'b => option('a),
        ~failureMessage="Parsing Failed",
        ~failureContentType,
        calc: t('a),
      ) =>
    andThen(
      parser,
      ~failureCode=Failure.Status400,
      ~failureMessage,
      ~failureContentType,
      calc,
    );

  let parseJson =
      (
        decoder: Js.Json.t => option('b),
        ~failureMessage="Invalid JSON format.",
        ~failureContentType="application/json",
        calc,
      ) =>
    parse(decoder, ~failureMessage, ~failureContentType, calc);

  //----------------------------------------------------------------------------
  // SENDERS

  /* Stringifies the Json object, sets content type to application/json and
   * returns the response */
  let sendJson = (~code=Success.Status200, body: t(Js.Json.t)) =>
    body
    |> map(Js.Json.stringify)
    |> map(value => Some(value))
    |> setContentType("application/json")
    |> send(code);

  let sendHtml = (~code=Success.Status200, body) =>
    body |> setContentType("text/html") |> send(code);

  let sendText = (text: string, ~code=Success.Status200, body) =>
    body
    |> map(_ => Some(text))
    |> setContentType("text/plain")
    |> send(code);
};

//------------------------------------------------------------------------------
// APP
module App = {
  type t = Request.t => Response.t;
  type routerApp = Builder.t(string) => Response.t;
  type spec = Spec.t(routerApp => routerApp, routerApp);

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
        Status.Failure.Status400,
        convertedHeaders,
        ~message="No method",
      )
      |> Response.convert
    };
  };

  //----------------------------------------------------------------------------
  // MAKE
  let makeSpec = (spec: spec, ~notFound="<h1>404 - Not found</h1>"): t =>
    request =>
      Spec.parse(spec, request.method, request.path, request.body)
      ->Belt.Option.map(_, handler => handler(Builder.start(request)))
      ->Belt.Option.getWithDefault(
          _,
          Response.fail(
            Status.Failure.Status404,
            Header.Map.singleton("Content-Type", "application/html"),
            ~message=notFound,
          ),
        );

  let start = (port, server) => Unsafe.server(port, unsafeHandle(server));

  /** start a HTTPS server */
  let startSecure = (~port, ~keyFilepath, ~certificateFilepath, server) =>
    Unsafe.secureServer(
      port,
      keyFilepath,
      certificateFilepath,
      unsafeHandle(server),
    );
};