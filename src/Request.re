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

type uriState =
  | Unparsed(string)
  | Parsed(list(string));

type queryState =
  | Unparsed(string)
  | NoQuery
  | Parsed(querySet);

type t = {
  url: string,
  offset: int,
  length: int,
  queries: queryState,
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

let compose = (f, g, x) => f(g(x));

//------------------------------------------------------------------------------
// PARSING
let decodeBody: type a. (accepts(a), t) => option(a) =
  (accepts, request) =>
    Belt.Map.get(accepts, request.contentType)
    ->Belt.Option.flatMap(_, decoder => decoder(request.rawBody));

let parseQueries = req => {
  switch (req.queries) {
  | Unparsed(str) => {
      ...req,
      queries:
        Parsed(queries(str, req.offset, req.length, Belt.Map.String.empty)),
    }
  | NoQuery => req
  | Parsed(q) => req
  };
};
let splitAtQuestionMark = Js.String.splitAtMost("?", ~limit=1);

// let parseUri = (req: t) =>
//   switch (req.url) {
//   | Unparsed(str) =>
//     str
//     |> Js.String.split("/")
//     |> Array.to_list
//     |> (url => {...req, url: Parsed(url)})
//   | Parsed(r) => req
//   };

// let route = (request: t) => {
//   let sep = splitAtQuestionMark(request.url) |> Array.to_list;
//   switch (sep) {
//   | [url, queries] =>
//     Result.Ok(((request.method, parseUri(url)), Unparsed(queries)))
//   | [url] => Result.Ok(((request.method, parseUri(url)), NoQuery))
//   | _ => Result.Failed("Not found", Status.NotFound404, MediaType.Html)
//   };
// };

let rec query: type a. (string, string => option(a), t) => option(a) =
  (str, parse, req) => {
    switch (req.queries) {
    | Unparsed(str) => query(str, parse, parseQueries(req))
    | NoQuery => None
    | Parsed(q) =>
      switch (Belt.Map.String.get(q, str)) {
      | Some(unparsed) => parse(unparsed)
      | None => None
      }
    };
  };

module Optional = {
  let string = s => Some(s);
  let int = Belt.Int.fromString;
} /* }*/ /*   rawBody: ""*/ /*   encoding: Encoding.Ascii*/ /*   method: HttpMethod.GET*/;

// let mock = (uri, body, method) => {
//   url: uri,
//   offset: 0,
//   length: String.length(uri),
//   queries: NoQuery,
//   contentType: MediaType.Plain,
//   headers: Header.Map.empty,
//   accept: MediaType.Plain,
//   method,
//   rawBody: body,
//   encoding: Encoding.Ascii,
// };

// let mockPost = (uri, body) => {
//   url: uri,
//   offset: 0,
//   length: String.length(uri),
//   queries: NoQuery,
//   contentType: MediaType.Plain,
//   headers: Header.Map.empty,
//   accept: MediaType.Plain,
//   method: HttpMethod.POST,
//   rawBody: body,
//   encoding: Encoding.Ascii,
// };

let mockGet = uri => {
  url: uri,
  offset: 0,
  length: String.length(uri),
  queries: NoQuery,
  contentType: MediaType.Plain,
  headers: Header.Map.empty,
  accept: MediaType.Plain,
  rawBody: "hi",
  encoding: Encoding.Ascii,
  method: HttpMethod.GET,
};