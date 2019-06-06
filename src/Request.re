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

let contramap: type a b. (a => b, t(b)) => t(a => b) =
  (f, content) =>
    map((x, x) => x, content)
    |> (newCont => {...content, arguments: compose(f, newCont.arguments)});

let compose = (f, x) => f(x);

//------------------------------------------------------------------------------
// PARSING
let decodeBody: type a b. (accepts(a), t(a => b)) => option(t(b)) =
  (accepts, req) =>
    Belt.Map.get(accepts, req.contentType)
    ->Belt.Option.flatMap(_, decoder => decoder(req.rawBody))
    ->Belt.Option.map(_, parsedBody =>
        {...req, arguments: compose(req.arguments, parsedBody)}
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