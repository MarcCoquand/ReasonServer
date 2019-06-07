module MediaComparer =
  Belt.Id.MakeComparable({
    type t = MediaType.t;
    let cmp = compare;
  });
type querySet = Belt.Map.String.t(string);
type accepts('a) =
  Belt.Map.t(MediaComparer.t, string => option('a), MediaComparer.identity);
type contenttype('a) =
  Belt.Map.t(MediaType.t, 'a => string, MediaComparer.t);

let id: type a. a => a = x => x;
type nohandler =
  | NoHandler;

type t('a) = {
  url: string,
  offset: int,
  length: int,
  arguments: 'a,
  queries: option(querySet),
  contentType: MediaType.t,
  headers: Header.Map.t(string),
  accept: MediaType.t,
  method: HttpMethod.t,
  rawBody: string,
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
  (f, content) => {...content, arguments: f(content.arguments)};

let compose = (f, g, x) => f(g(x));

let pure = (value, request: t(nohandler)) => {...request, arguments: value};

let apply: type a b. (a, t(a => b)) => t(b) =
  (value, request) => {...request, arguments: request.arguments(value)};

let applyCombine: type a b. (t(a => b), t(a)) => t(b) =
  (request, value) => {
    ...value,
    arguments: request.arguments(value.arguments),
  };

let compose = (f, x) => f(x);

//------------------------------------------------------------------------------
// PARSING
let decodeBody: type a b. (accepts(a), t(a => b)) => option(t(b)) =
  (accepts, request) =>
    Belt.Map.get(accepts, request.contentType)
    ->Belt.Option.flatMap(_, decoder => decoder(request.rawBody))
    ->Belt.Option.map(_, decodedBody => apply(decodedBody, request));

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
        arguments: compose(req.arguments, parse(unparsed)),
        queries: Some(queries),
      }
    | None => {
        ...req,
        url: "",
        length: 0,
        queries: Some(queries),
        arguments: compose(req.arguments, None),
      }
    };
  };

module Optional = {
  let string = s => Some(s);
  let int = Belt.Int.fromString;
};

let mock = (uri, body, method) => {
  url: uri,
  offset: 0,
  length: String.length(uri),
  arguments: id,
  queries: None,
  contentType: MediaType.Plain,
  headers: Header.Map.empty,
  accept: MediaType.Plain,
  method,
  rawBody: body,
  encoding: Encoding.Ascii,
};

let mockPost = (uri, body) => {
  url: uri,
  offset: 0,
  length: String.length(uri),
  arguments: NoHandler,
  queries: None,
  contentType: MediaType.Plain,
  headers: Header.Map.empty,
  accept: MediaType.Plain,
  method: HttpMethod.POST,
  rawBody: body,
  encoding: Encoding.Ascii,
};

let mockGet = uri => {
  url: uri,
  offset: 0,
  length: String.length(uri),
  arguments: id,
  queries: None,
  contentType: MediaType.Plain,
  headers: Header.Map.empty,
  accept: MediaType.Plain,
  method: HttpMethod.GET,
  rawBody: "",
  encoding: Encoding.Ascii,
};