// Shameless port from
// https://github.com/elm/package.elm-lang.org/blob/master/src/backend/Server/Router.hs

//------------------------------------------------------------------------------
// TYPES
type t('a, 'b) =
  | Top: t('a, 'a)
  | Exact(string): t('a, 'a)
  | Custom(string => option('a)): t('a => 'b, 'b)
  | Slash(t('a, 'b), t('b, 'c)): t('a, 'c)
  | Map('a, t('a, 'b)): t('b => 'c, 'c)
  | Integer: t(int => 'a, 'a)
  | OneOf(list(t('a, 'b))): t('a, 'b)
  | Method(HttpMethod.t): t('a, 'a)
  | Body(Status.code, string, string => option('a)): t('a => 'b, 'b)
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
module Progress = {
  type t('a) =
    | Parsing(list('a))
    | Failed(Status.code, string);

  let map: type a b. (a => b, t(a)) => t(b) =
    (f, l) =>
      switch (l) {
      | Parsing(states) => Parsing(List.map(f, states))
      | Failed(code, msg) => Failed(code, msg)
      };

  let concat: type a. t(t(a)) => t(a) =
    l =>
      switch (l) {
      | Parsing(states) =>
        List.fold_right(
          (results, n) =>
            switch (results) {
            | Parsing(states) =>
              switch (n) {
              | Parsing(nStates) => Parsing(List.append(nStates, states))
              | Failed(code, msg) => Failed(code, msg)
              }
            | Failed(code, msg) => Failed(code, msg)
            },
          states,
          Parsing([]),
        )
      | Failed(code, msg) => Failed(code, msg)
      };
  let concatMap: type a b. (a => t(b), t(a)) => t(b) =
    (f, l) => map(f, l) |> concat;
};

module Chomp = {
  //----------------------------------------------------------------------------
  // CHOMP EXACT
  let rec isSubString = (small, big, offset, i, smallLen) =>
    if (i == smallLen) {
      true;
    } else if (small.[i] == big.[offset + i]) {
      isSubString(small, big, offset, i + 1, smallLen);
    } else {
      false;
    };

  let exact = (small, big, offset, length) => {
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

  //----------------------------------------------------------------------------
  // CHOMP INT
  let charDigitToInt = (str): option(int) =>
    switch (str) {
    | '0'..'9' => Some(int_of_char(str) - 48)
    | _ => None
    };

  let rec intHelp = (url, offset, length, n) =>
    if (length == 0) {
      (offset, length, n);
    } else {
      let word = url.[offset];
      switch (charDigitToInt(word)) {
      | Some(i) => intHelp(url, offset + 1, length - 1, n * 10 + i)
      | None =>
        word == '/' || word == '&' || word == '=' || word == '?'
          ? (offset + 1, length - 1, n) : (offset, length, n)
      };
    };

  let int = (url, offset, length) =>
    if (length == 0) {
      (offset, length, 0);
    } else {
      let word = url.[offset];
      switch (charDigitToInt(word)) {
      | Some(i) => intHelp(url, offset + 1, length - 1, i)
      | None => (offset, length, 0)
      };
    };

  //----------------------------------------------------------------------------
  // CHOMP SEGMENT
  let rec segment = (url, offset, length) =>
    if (length == 0) {
      (offset, offset, length);
    } else if (url.[offset] == '/'
               || url.[offset] == '&'
               || url.[offset] == '='
               || url.[offset] == '?') {
      (offset, offset + 1, length - 1);
    } else {
      segment(url, offset + 1, length - 1);
    };
  let extractValue = (str, start, upto) =>
    String.sub(str, start, upto - start);

  //----------------------------------------------------------------------------
  // CHOMP QUERIES
  // Query parameters are unordered while argument list is ordered.
  // Thus we need to parse all of them before checking the values are of a
  // correct type.
  let rec queries = (url, offset, length, set): querySet => {
    let (endOffsetKey, offsetKey, lengthKey) = segment(url, offset, length);

    if (offsetKey == offset) {
      set;
    } else {
      let key = extractValue(url, offset, endOffsetKey);
      let (endOffsetVal, nextOffset, nextLength) =
        segment(url, offsetKey, lengthKey);
      if (nextOffset == offsetKey) {
        set;
      } else {
        let queryValue = extractValue(url, offsetKey, endOffsetVal);
        let newSet = Belt.Map.String.set(set, key, queryValue);
        queries(url, nextOffset, nextLength, newSet);
      };
    };
  };
};

module Query = {
  type t('a) = string => option('a);
  // All strings are valid
  let text = value => Some(value);
  let int = Belt.Int.fromString;
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

//------------------------------------------------------------------------------
// PARSING
let rec attempt: type a b. (t(a, b), state(a)) => Progress.t(state(b)) =
  (route, state) => {
    switch (route) {
    | Top =>
      if (state.length == 0) {
        Parsing([state]);
      } else {
        Parsing([]);
      }

    | Exact(str) =>
      let (newOffset, newLength) =
        Chomp.exact(str, state.url, state.offset, state.length);
      if (newOffset == (-1)) {
        Parsing([]);
      } else {
        Parsing([{...state, offset: newOffset, length: newLength}]);
      };
    | Integer =>
      let (newOffset, newLength, number) =
        Chomp.int(state.url, state.offset, state.length);
      if (newOffset <= state.offset) {
        Parsing([]);
      } else {
        Parsing([
          {
            ...state,
            length: newLength,
            offset: newOffset,
            value: state.value(number),
          },
        ]);
      };
    | Body(code, errorMessage, parser) =>
      switch (parser(state.body)) {
      | Some(result) => Parsing([{...state, value: state.value(result)}])
      | None => Failed(code, errorMessage)
      }

    | Optional(str, parse) =>
      let queries =
        Belt.Option.getWithDefault(
          state.queries,
          Chomp.queries(
            state.url,
            state.offset,
            state.length,
            Belt.Map.String.empty,
          ),
        );

      switch (Belt.Map.String.get(queries, str)) {
      | None =>
        Parsing([
          {
            ...state,
            url: "",
            length: 0,
            queries: Some(queries),
            value: state.value(None),
          },
        ])
      | Some(unparsed) =>
        Parsing([
          {
            ...state,
            url: "",
            length: 0,
            value: state.value(parse(unparsed)),
            queries: Some(queries),
          },
        ])
      };
    | Method(ofType) =>
      if (state.method == ofType) {
        Parsing([state]);
      } else {
        Parsing([]);
      }

    | Custom(checker) =>
      let (endOffset, newOffset, newLength) =
        Chomp.segment(state.url, state.offset, state.length);
      if (endOffset == state.offset) {
        Parsing([]);
      } else {
        let subString =
          String.sub(state.url, state.offset, endOffset - state.offset);
        switch (checker(subString)) {
        | None => Parsing([])
        | Some(nextValue) =>
          Parsing([
            {
              ...state,
              offset: newOffset,
              length: newLength,
              value: state.value(nextValue),
            },
          ])
        };
      };
    | Slash(before, after) =>
      Progress.concatMap(attempt(after), attempt(before, state))

    | Map(subValue, subParser) =>
      Progress.map(
        mapHelp(state.value),
        attempt(subParser, {...state, value: subValue}),
      )
    | OneOf(routes) =>
      Progress.concatMap(p => attempt(p, state), Parsing(routes))
    };
  };

type result('a) =
  | Success('a)
  | Failed(Status.code, string);
let rec parseHelp = results => {
  switch (results) {
  | Progress.Parsing([]) => Failed(Status.NotFound404, "Not found")
  | Progress.Parsing([state, ...restStates]) =>
    if (state.length == 0) {
      Success(state.value);
    } else {
      parseHelp(Progress.Parsing(restStates));
    }
  | Progress.Failed(code, msg) => Failed(code, msg)
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
//      get [top |> get(index),
//           is "api" >- is "user" >- int |> get(userHandle)]
module Required = {
  let top = Top;
  let is = (str: string) => Exact(str);
  let int = Integer;
  let text = Custom(value => Some(value));

  let custom = (f: string => option('a)): t('a => 'b, 'b) => Custom(f);
  let map = (toMap: 'a, route: t('a, 'b)): t('b => 'c, 'c) =>
    Map(toMap, route);
  "user";
  let oneOf = (l: list(t('a, 'b))) => OneOf(l);

  let jsonBody =
      (
        ~failureCode=Status.BadRequest400,
        ~failureMessage="Invalid Json body",
        parser: Js.Json.t => 'a,
      ) =>
    Body(
      failureCode,
      failureMessage,
      str => Belt.Option.map(Json.parse(str), parser),
    );
};
// let (<&>) = (route, (str, f)) =>
//   Slash(
//     Slash(route, Optional(str, value => parseString(f, value))),
//     Optional(str, value => parseString(f, value)),
//   );

let query = (str, f) => Optional(str, f);
//------------------------------------------------------------------------------
// ROUTER HTTP METHOD COMBINATORS
// Left associative operator. Use when you don't care about http method
let (==>) = (route: t('a, 'b), handler: 'a): t('b => 'c, 'c) =>
  Map(handler, route);
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
let (>-) = (f, g): t('a, 'c) => Slash(f, g);