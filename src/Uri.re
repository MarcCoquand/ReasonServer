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
  {...state, arguments: func(state.arguments)}
  |> (
    value => {
      value;
    }
  );

//------------------------------------------------------------------------------
// PARSING
let rec attempt: type a b. (t(a, b), Request.t(a)) => list(Request.t(b)) =
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
            arguments: state.arguments(number),
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
              arguments: state.arguments(nextValue),
            },
          ]
        };
      };
    | Slash(before, after) =>
      concatMap(attempt(after), attempt(before, state))

    | Map(subValue, subParser) =>
      List.map(
        mapHelp(state.arguments),
        attempt(subParser, {...state, arguments: subValue}),
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
let (==>) = (a, b) => Map(a, b);

let rec parseHelp = (results: list(Request.t('a))) => {
  switch (results) {
  | [] => None
  | [state, ...restStates] =>
    if (state.length == 0) {
      Some(state.arguments);
    } else {
      parseHelp(restStates);
    }
  };
};

let parse: type a b. (t(a => b, b), Request.t(a => b)) => Result.t(b) =
  (route, req) => {
    let firstDropped = String.sub(req.url, 1, String.length(req.url) - 1);
    Result.attempt(
      ~message="Not found",
      ~code=Status.NotFound404,
      ~contenttype=MediaType.Plain,
      (v: Request.t(a => b)) =>
        attempt(route, {...v, url: firstDropped, length: v.length - 1})
        |> parseHelp,
      req,
    );
  };