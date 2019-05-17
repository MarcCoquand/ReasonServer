// Shameless port from
// https://github.com/elm/package.elm-lang.org/blob/master/src/backend/Server/Router.hs

type t('a, 'b) =
  | Top: t('a, 'a)
  | Exact(string): t('a, 'a)
  | Custom(string => option('a)): t('a => 'b, 'b)
  | Slash(t('a, 'b), t('b, 'c)): t('a, 'c)
  | Map('a, t('a, 'b)): t('b => 'c, 'c)
  | Integer: t(int => 'a, 'a)
  | OneOf(list(t('a, 'b))): t('a, 'b)
  | Method(HttpMethod.t): t('a, 'a)
  | Body(string => option('a)): t('a => 'b, 'b)
  | Optional(string, string => option('a)): t(option('a) => 'b, 'b);

type querySet = Belt.Map.String.t(string);
type state('a) = {
  url: string,
  offset: int,
  length: int,
  value: 'a,
  queries: option(querySet),
  method: HttpMethod.t,
  body: string,
};
let tap = (str, value) => {
  Js.log2(str ++ ": ", value);
  value;
};

//------------------------------------------------------------------------------
// HELPERS
let mapHelp = (func, state) =>
  {...state, value: func(state.value)}
  |> (
    value => {
      value;
    }
  );
let concatMap = (f, l) => List.concat(List.map(f, l));
let extractValue = (str, start, upto) =>
  String.sub(str, start, upto - start);

//------------------------------------------------------------------------------
// CHOMP EXACT
let rec isSubString = (small, big, offset, i, smallLen) =>
  if (i == smallLen) {
    true;
  } else if (small.[i] == big.[offset + i]) {
    isSubString(small, big, offset, i + 1, smallLen);
  } else {
    false;
  };

let chompExact = (small, big, offset, length) => {
  let smallLen = String.length(small);
  if (length < smallLen || smallLen == 0) {
    ((-1), length);
  } else if (!isSubString(small, big, offset, 0, smallLen)) {
    ((-1), length);
  } else {
    let newOffset = offset + smallLen;
    let newLength = length - smallLen;
    if (newLength == 0) {
      (newOffset, newLength);
    } else if (big.[newOffset] == '/'
               || big.[newOffset] == '&'
               || big.[newOffset] == '='
               || big.[newOffset] == '?') {
      (newOffset + 1, newLength - 1);
    } else {
      ((-1), length);
    };
  };
};

//------------------------------------------------------------------------------
// CHOMP SEGMENT
let rec chompSegment = (url, offset, length) =>
  if (length == 0) {
    (offset, offset, length);
  } else if (url.[offset] == '/'
             || url.[offset] == '&'
             || url.[offset] == '='
             || url.[offset] == '?') {
    (offset, offset + 1, length - 1);
  } else {
    chompSegment(url, offset + 1, length - 1);
  };

//------------------------------------------------------------------------------
// CHOMP QUERIES
// Query parameters are unordered while argument list is ordered. Thus we parse
// all the parameters and check if they contain each.
let rec chompQueries = (url, offset, length, set): querySet => {
  let (endOffsetKey, offsetKey, lengthKey) =
    chompSegment(url, offset, length);

  if (offsetKey == offset) {
    set;
  } else {
    let key = extractValue(url, offset, endOffsetKey);
    let (endOffsetVal, nextOffset, nextLength) =
      chompSegment(url, offsetKey, lengthKey);
    if (nextOffset == offsetKey) {
      set;
    } else {
      let queryValue = extractValue(url, offsetKey, endOffsetVal);
      let newSet = Belt.Map.String.set(set, key, queryValue);
      chompQueries(url, nextOffset, nextLength, newSet);
    };
  };
};

//------------------------------------------------------------------------------
// CHOMP INT
let charDigitToInt = (str): option(int) =>
  switch (str) {
  | '0'..'9' => Some(int_of_char(str) - 48)
  | _ => None
  };

let rec chompIntHelp = (url, offset, length, n) =>
  if (length == 0) {
    (offset, length, n);
  } else {
    let word = url.[offset];
    switch (charDigitToInt(word)) {
    | Some(i) => chompIntHelp(url, offset + 1, length - 1, n * 10 + i)
    | None =>
      word == '/' || word == '&' || word == '=' || word == '?'
        ? (offset + 1, length - 1, n) : (offset, length, n)
    };
  };

let chompInt = (url, offset, length) =>
  if (length == 0) {
    (offset, length, 0);
  } else {
    let word = url.[offset];
    switch (charDigitToInt(word)) {
    | Some(i) => chompIntHelp(url, offset + 1, length - 1, i)
    | None => (offset, length, 0)
    };
  };
//------------------------------------------------------------------------------
// PARSING
let rec attempt: type a b. (t(a, b), state(a)) => list(state(b)) =
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
        chompExact(str, state.url, state.offset, state.length);
      if (newOffset == (-1)) {
        [];
      } else {
        [{...state, offset: newOffset, length: newLength}];
      };
    | Integer =>
      let (newOffset, newLength, number) =
        chompInt(state.url, state.offset, state.length);
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
    | Body(parser) =>
      switch (parser(state.body)) {
      | Some(result) => [{...state, value: state.value(result)}]
      | None => []
      }

    | Optional(str, parse) =>
      let queries =
        Belt.Option.getWithDefault(
          state.queries,
          chompQueries(
            state.url,
            state.offset,
            state.length,
            Belt.Map.String.empty,
          ),
        );

      switch (Belt.Map.String.get(queries, str)) {
      | None => [
          {
            ...state,
            url: "",
            length: 0,
            queries: Some(queries),
            value: state.value(None),
          },
        ]
      | Some(unparsed) => [
          {
            ...state,
            url: "",
            length: 0,
            value: state.value(parse(unparsed)),
            queries: Some(queries),
          },
        ]
      };
    | Method(ofType) =>
      if (state.method == ofType) {
        [state];
      } else {
        [];
      }

    | Custom(checker) =>
      let (endOffset, newOffset, newLength) =
        chompSegment(state.url, state.offset, state.length);
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

let rec parseHelp = states => {
  switch (states) {
  | [] => None
  | [state, ...restStates] =>
    if (state.length == 0) {
      Some(state.value);
    } else {
      parseHelp(restStates);
    }
  };
};
let parse = (route: t('a => 'a, 'a), method, path, body) => {
  let id: type a. a => a = a => a;
  let firstDropped = String.sub(path, 1, String.length(path) - 1);
  parseHelp(
    attempt(
      route,
      {
        url: firstDropped,
        offset: 0,
        length: String.length(firstDropped),
        method,
        value: id,
        queries: None,
        body,
      },
    ),
  );
};
let parseString = (route: t('a => 'b, 'b), path) => {
  let id: type a. a => a = a => a;
  parseHelp(
    attempt(
      route,
      {
        url: path,
        offset: 0,
        length: String.length(path),
        method: HttpMethod.GET,
        value: id,
        queries: None,
        body: "",
      },
    ),
  );
};

//------------------------------------------------------------------------------
// ROUTER COMBINATORS
//
// EXAMPLE
// let router =
//      get [top ==> index,
//           is "api" ^/^ is "user" ==> userHandle]
let top = Top;
let is = (str: string) => Exact(str);
let int = Integer;
let text = Custom(value => Some(value));
let custom = (f: string => option('a)): t('a => 'b, 'b) => Custom(f);
let (>-) = (f, g): t('a, 'c) => Slash(f, g);
let map = (toMap: 'a, route: t('a, 'b)): t('b => 'c, 'c) =>
  Map(toMap, route);
"user";
let oneOf = (l: list(t('a, 'b))) => OneOf(l);

// Left associative operator. Use when you don't care about http method
let (==>) = (route: t('a, 'b), handler: 'a): t('b => 'c, 'c) =>
  Map(handler, route);

let jsonBody = (parser: Js.Json.t => 'a) =>
  Body(str => Belt.Option.map(Json.parse(str), parser));

// let (<&>) = (route, (str, f)) =>
//   Slash(
//     Slash(route, Optional(str, value => parseString(f, value))),
//     Optional(str, value => parseString(f, value)),
//   );
let query = (str, f) => Optional(str, value => parseString(f, value));

// let options = (l: list(t(option('b) => 'a, 'a))) =>
//   Optional(str, map(v => Some(v), f));

//------------------------------------------------------------------------------
// ROUTER HTTP METHOD COMBINATORS
let get = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.GET), route));
let post = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.POST), route));
let delete = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.DELETE), route));
let put = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.PUT), route));
let update = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.UPDATE), route));
let methodHead = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.HEAD), route));
let methodOption = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.OPTION), route));
let methodConnect = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.CONNECT), route));
let methodTrace = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.TRACE), route));
let patch = (handler, route) =>
  Map(handler, Slash(Method(HttpMethod.PATCH), route));