module Response = {
  type content('a) = {
    code: Status.code,
    headers: Header.Map.t(string),
    contentType: MediaType.t,
    body: 'a,
    encoding: Encoding.t,
  };
  let compose = (f, g, x) => f(g(x));

  type t('a) = content('a => string);

  // Contramap the response
  let applyEncoder: type a b. (a => b, t(b)) => t(a) =
    (cf, res) => {...res, body: compose(res.body, cf)};

  let setCode = (code: Status.code, res): t('a) => {...res, code};

  let setContentType = (mediaType, res): t('a) => {
    ...res,
    contentType: mediaType,
  };
  let json = (encoder: 'a => Js.Json.t, res: t(string)): t('a) =>
    applyEncoder(Js.Json.stringify, res)
    |> applyEncoder(encoder)
    |> setContentType(MediaType.Json);
};
module MediaComparer =
  Belt.Id.MakeComparable({
    type t = Cause.MediaType.t;
    let cmp = compare;
  });

type querySet = Belt.Map.String.t(string);
type accepts('a) =
  Belt.Map.t(MediaType.t, string => option('a), MediaComparer.t);
type contenttype('a) =
  Belt.Map.t(MediaType.t, 'a => string, MediaComparer.t);

module Request = {
  type t('a) = {
    url: string,
    offset: int,
    length: int,
    value: 'a,
    queries: option(querySet),
    contentType: MediaType.t,
    headers: Header.Map.t(string),
    accept: MediaType.t,
    method: HttpMethod.t,
    rawBody: string,
    code: Status.code,
    encoding: Encoding.t,
  };
  let rec queries = (url, offset, length, set): querySet => {
    // Take a segment from the right, this is the value
    // Check for =
    // else if return original
    // Take a segment from the right, this is the key
    // if & then repeat again
    // else if ? then done, return original offset and length - chomped
    // else if return origial
    let (endOffsetKey, offsetKey, lengthKey) =
      Chomp.segment(url, offset, length);

    if (offsetKey == offset) {
      set;
    } else {
      let key = Chomp.extractValue(url, offset, endOffsetKey);
      let (endOffsetVal, nextOffset, nextLength) =
        Chomp.segment(url, offsetKey, lengthKey);
      if (nextOffset == offsetKey) {
        set;
      } else {
        let queryValue = Chomp.extractValue(url, offsetKey, endOffsetVal);
        let newSet = Belt.Map.String.set(set, key, queryValue);
        queries(url, nextOffset, nextLength, newSet);
      };
    };
  };
  let map: type a b. (a => b, t(a)) => t(b) =
    (f, content) => {...content, value: f(content.value)};

  let compose = (f, g, x) => f(g(x));

  //------------------------------------------------------------------------------
  // PARSING
  let decodeBody: type a b. (accepts(a), t(a => b)) => option(t(b)) =
    (accepts, req) =>
      Belt.Map.get(accepts, req.contentType)
      ->Belt.Option.flatMap(_, decoder => decoder(req.rawBody))
      ->Belt.Option.map(_, parsedBody =>
          {...req, value: req.value(parsedBody)}
        );

  let query:
    type a b. (string, string => option(a), t(option(a) => b)) => t(b) =
    (str, parse, req) => {
      let queries =
        Belt.Option.getWithDefault(
          req.queries,
          queries(req.url, req.offset, req.length, Belt.Map.String.empty),
        );

      switch (Belt.Map.String.get(queries, str)) {
      | Some(unparsed) => {
          ...req,
          url: "",
          length: 0,
          value: req.value(parse(unparsed)),
          queries: Some(queries),
        }
      | None => {
          ...req,
          url: "",
          length: 0,
          queries: Some(queries),
          value: None |> req.value,
        }
      };
    };

  let exact = (str, req) => {
    let (newOffset, newLength) =
      Chomp.exact(str, req.url, req.offset, req.length);
    if (newOffset == (-1)) {
      None;
    } else {
      Some({...req, offset: newOffset, length: newLength});
    };
  };

  let top = req =>
    if (req.length == 0) {
      Some(req);
    } else {
      None;
    };

  let integer = req => {
    let (newOffset, newLength, number) =
      Chomp.int(req.url, req.offset, req.length);
    if (newOffset <= req.offset) {
      None;
    } else {
      Some({
        ...req,
        length: newLength,
        offset: newOffset,
        value: req.value(number),
      });
    };
  };
};

module Spec = {
  open Arrow;
  type t('a, 'b) = Arrow.t(Request.t('a), Response.t('b));
  type complete('a, 'b) = Arrow.t(Request.t('a), (Response.t('b), 'b));
  let id: type a. a => a = x => x;

  let throw =
      (
        ~message="Internal server error",
        ~code=Status.Error500,
        ~method=MediaType.Html,
      )
      : Response.content(string) => {
    code,
    headers: Header.Map.empty,
    contentType: method,
    body: message,
    encoding: Encoding.Ascii,
  };

  // Run an operation that might fail and invoke the correct error response.
  let attempt =
      (
        ~message="Internal server error",
        ~code=Status.Error500,
        ~contenttype=MediaType.Html,
        f,
        value,
      ) =>
    f(value)
    |> (
      res =>
        switch (res) {
        | Some(success) => Ok(success)
        | None => Failed(message, code, contenttype)
        }
    );

  // Reads the request and sets the appropriate encoder
  let accept =
      (contentTypes: contenttype('a), builder: t('a, string)): t('a, 'b) => {
    let encoder = (req: Request.t('a)) =>
      attempt(
        ~message="Unsupported Media Type: " ++ MediaType.toString(req.accept),
        ~code=Status.UnsupportedMediaType415,
        ~contenttype=MediaType.Html,
        Belt.Map.get(contentTypes),
        req.accept,
      );
    arr(encoder) &&& builder >>> merge(Response.applyEncoder);
  };

  let query = (str, parse, builder: t(option('b) => 'a, 'c)) =>
    (req => Ok(Request.query(str, parse, req))) ^>> builder;

  // Reads the request and decodes the body using one of the given decoders from
  // the set
  let contentType =
      (~errorContenttype=MediaType.Plain, contentTypes: accepts('a), builder)
      : t('b, 'c) => {
    let decoder = (req: Request.t('a => 'a)) =>
      attempt(
        ~message=
          "Could not parse body with content type: "
          ++ MediaType.toString(req.contentType),
        ~code=Status.BadRequest400,
        ~contenttype=errorContenttype,
        Request.decodeBody(contentTypes),
        req,
      );
    decoder ^>> builder;
  };
  module Uri = {
    type t('a, 'b) =
      | Top: t('a, 'a)
      | Exact(string): t('a, 'a)
      | Custom(string => option('a)): t('a => 'b, 'b)
      | Slash(t('a, 'b), t('b, 'c)): t('a, 'c)
      | Map('a, t('a, 'b)): t('b => 'c, 'c)
      | Integer: t(int => 'a, 'a)
      | OneOf(list(t('a, 'b))): t('a, 'b);

    let concatMap = (f, l) => List.map(f, l) |> List.concat;
    //------------------------------------------------------------------------------
    // HELPERS
    let mapHelp = (func, state: Request.t('a)) =>
      {...state, value: func(state.value)}
      |> (
        value => {
          value;
        }
      );

    //------------------------------------------------------------------------------
    // PARSING
    let rec attempt:
      type a b. (t(a, b), Request.t(a)) => list(Request.t(b)) =
      (route, state) => {
        switch (route) {
        | Top =>
          if (state.length == 0) {
            [state];
          } else {
            [];
          }

        | Exact(str) =>
          let (newOffset, newLength) =
            Chomp.exact(str, state.url, state.offset, state.length);
          if (newOffset == (-1)) {
            [];
          } else {
            [{...state, offset: newOffset, length: newLength}];
          };
        | Integer =>
          let (newOffset, newLength, number) =
            Chomp.int(state.url, state.offset, state.length);
          if (newOffset <= state.offset) {
            [];
          } else {
            [
              {
                ...state,
                length: newLength,
                offset: newOffset,
                value: state.value(number),
              },
            ];
          };
        | Custom(checker) =>
          let (endOffset, newOffset, newLength) =
            Chomp.segment(state.url, state.offset, state.length);
          if (endOffset == state.offset) {
            [];
          } else {
            let subString =
              String.sub(state.url, state.offset, endOffset - state.offset);
            switch (checker(subString)) {
            | None => []
            | Some(nextValue) => [
                {
                  ...state,
                  offset: newOffset,
                  length: newLength,
                  value: state.value(nextValue),
                },
              ]
            };
          };
        | Slash(before, after) =>
          concatMap(attempt(after), attempt(before, state))

        | Map(subValue, subParser) =>
          List.map(
            mapHelp(state.value),
            attempt(subParser, {...state, value: subValue}),
          )
        | OneOf(routes) => concatMap(p => attempt(p, state), routes)
        };
      };

    let top = Top;
    let is = (str: string) => Exact(str);
    let int = Integer;
    let text = Custom(value => Some(value));

    let custom = (f: string => option('a)): t('a => 'b, 'b) => Custom(f);
    let map = (toMap: 'a, route: t('a, 'b)): t('b => 'c, 'c) =>
      Map(toMap, route);
    "user";
    let oneOf = (l: list(t('a, 'b))) => OneOf(l);
    let (>-) = (a, b) => Slash(a, b);

    let rec parseHelp = (results: list(Request.t('a))) => {
      switch (results) {
      | [] => None
      | [state, ...restStates] =>
        if (state.length == 0) {
          Some(state.value);
        } else {
          parseHelp(restStates);
        }
      };
    };
  };
  let parseRequest:
    type a b. (Uri.t(a => b, b), Request.t(a => b)) => result(b) =
    (route, req) => {
      let firstDropped = String.sub(req.url, 1, String.length(req.url) - 1);
      attempt(
        ~message="Not found",
        ~code=Status.NotFound404,
        ~contenttype=MediaType.Plain,
        (v: Request.t(a => b)) =>
          Uri.attempt(route, {...v, url: firstDropped, length: v.length - 1})
          |> Uri.parseHelp,
        req,
      );
    };

  let route = (router, builder) => parseRequest(router) ^>> builder;

  let handler =
      (handle: 'a => result('b), builder: t('a, 'b)): complete('a, 'b) =>
    builder &&& arr((req: Request.t('a)) => handle(req.value));

  let run:
    type a b.
      (t(a, b), Request.t(a), a => result(b)) => Response.content(string) =
    (builder, request, handle) => {
      let Run(parse) =
        builder &&& arr((req: Request.t(a)) => handle(req.value));

      parse(request)
      |> (
        result =>
          switch (result) {
          | Ok((response, unencodedBody)) => {
              ...response,
              // response.body = encoder
              body: response.body(unencodedBody),
            }
          | Failed(msg, code, content) =>
            throw(~message=msg, ~code, ~method=content)
          }
      );
    };
};