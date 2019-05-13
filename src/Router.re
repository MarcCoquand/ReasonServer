// Shameless port from
// https://github.com/elm/package.elm-lang.org/blob/master/src/backend/Server/Router.hs
type route('a, 'b) =
  | Top: route('a, 'a)
  | Exact(string): route('a, 'a)
  | Custom(string => option('a)): route('a => 'b, 'b)
  | Slash(route('a, 'b), route('b, 'c)): route('a, 'c)
  | Map('a, route('a, 'b)): route('b => 'c, 'c)
  | Integer: route(int => 'a, 'a)
  | OneOf(list(route('a, 'b))): route('a, 'b);

let top = Top;

let is = (str: string) => Exact(str);

let int = Integer;

let text = (f: string => 'a) => Custom(f);

let custom = (f: string => option('a)): route('a => 'b, 'b) => Custom(f);

let (-/-) = (f, g): route('a, 'c) => Slash(f, g);

let map = (toMap: 'a, route: route('a, 'b)): route('b => 'c, 'c) =>
  Map(toMap, route);

let oneOf = (l: list(route('a, 'b))) => OneOf(l);

let (==>) = (route: route('a, 'b), handler: 'a): route('b => 'c, 'c) =>
  Map(handler, route);

// Serve
type state('a) = {
  url: string,
  offset: int,
  length: int,
  value: 'a,
};

let mapHelp = (func, state) => {...state, value: func(state.value)};

let concatMap = (f, l) => List.concat(List.map(f, l));
// CHOMP EXACT

let rec isSubString = (small, big, offset, i, smallLen) =>
  if (i == smallLen) {
    true;
  } else if (small.[i] === big.[offset + i]) {
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
    } else if (big.[newOffset] === '/') {
      (newOffset + 1, newLength - 1);
    } else {
      ((-1), length);
    };
  };
};

// CHOMP INT

let rec chompIntHelp = (url, offset, length, n) =>
  if (length == 0) {
    (offset, length, n);
  } else {
    let word = url.[offset];
    switch (word) {
    | '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9' =>
      chompIntHelp(url, offset + 1, length - 1, n * 10 + int_of_char(word))
    | '/' => (offset + 1, length - 1, n)
    | _ => (offset, length, n)
    };
  };

let chompInt = (url, offset, length) =>
  if (length == 0) {
    (offset, length, 0);
  } else {
    let word = url.[offset];
    switch (word) {
    | '0'..'9' =>
      chompIntHelp(url, offset + 1, length - 1, int_of_char(word))
    | _ => (offset, length, 0)
    };
  };

// GET SEGMENT

let rec chompSegment = (url, offset, length) =>
  if (length == 0) {
    (offset, offset, length);
  } else if (url.[offset] == '/') {
    (offset, offset + 1, length - 1);
  } else {
    chompSegment(url, offset + 1, length - 1);
  };

let rec attempt: type a b. route(a, b) => state(a) => list(state(b)) =
  (route, state) =>
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
      concatMap(after->attempt, attempt(before, state))

    | Map(subValue, subParser) =>
      List.map(
        mapHelp(state.value),
        attempt(subParser, {...state, value: subValue}),
      )

    | OneOf(routes) => concatMap(p => attempt(p, state), routes)
    };

let rec parseHelp = states =>
  switch (states) {
  | [] => None
  | [state, ...restStates] =>
    if (state.length == 0) {
      Some(state.value);
    } else {
      parseHelp(restStates);
    }
  };

let parse = (route: route('a => 'a, 'a), path) => {
  let id: type a. a => a = a => a;
  parseHelp(
    attempt(
      route,
      {url: path, offset: 0, length: String.length(path), value: id},
    ),
  );
};
let serve = (path, router): option('a) => parse(router, path);